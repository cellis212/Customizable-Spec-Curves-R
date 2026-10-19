"""Unit tests for the Python port (run: python -m unittest discover -s python/tests)."""

import math
import sys
import unittest
from pathlib import Path

import numpy as np
import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from speccurve.engine import (MenuError, choice_grid, freeze_manifest, gmm_row_fields, gmm_varhac,  # noqa: E402
                              halton_point, newey_west_se, run_manifest, sample_sha256, validate_menu,
                              validate_results)


def series_data():
    rng = np.random.default_rng(7)
    rows = []
    for name in ("a", "b"):
        e = rng.normal(size=120)
        for t in range(120):
            rows.append({"series": name, "month": t + 1, "ret": 0.5 + e[t] + (0.3 * e[t - 1] if t else 0.0)})
    data = pd.DataFrame(rows)
    data["row_id"] = range(1, len(data) + 1)
    return data


def series_menu():
    return {
        "schema_version": 2, "version": "ts-test", "analysis_kind": "synthetic_demo", "estimator": "time_series_mean",
        "title": "Time-series test", "estimand": "Mean return", "row_id": "row_id", "unit": "series", "time": "month",
        "base": {"outcome": "ret", "vcov": {"type": "iid"}, "filters": []}, "confidence_level": 0.95,
        "dimensions": [
            {"name": "series", "label": "Series", "options": [
                {"id": "a", "label": "A", "filters": [{"variable": "series", "op": "==", "value": "a"}]},
                {"id": "b", "label": "B", "filters": [{"variable": "series", "op": "==", "value": "b"}]}]},
            {"name": "se", "label": "Uncertainty", "options": [
                {"id": "iid", "label": "i.i.d.", "vcov": {"type": "iid"}},
                {"id": "nw", "label": "Newey-West", "vcov": {"type": "newey_west", "lags": 4}}]},
            {"name": "window", "label": "Window", "options": [
                {"id": "all", "label": "All"},
                {"id": "late", "label": "Late", "filters": [{"variable": "month", "op": ">", "value": 60}]}]}],
        "sample_modes": ["available"],
        "exclusions": [{"match": {"se": "nw", "window": "late"}, "reason": "Too short for the lag choice."}],
        "preferred": {"series": "a", "se": "iid", "window": "all", "sample_mode": "available"},
    }


class EngineTests(unittest.TestCase):
    def test_grid_order_matches_r_expand_grid(self):
        grid, excluded = choice_grid(series_menu())
        # R's expand.grid varies the first dimension fastest.
        self.assertEqual(list(grid["series"][:4]), ["a", "b", "a", "b"])
        self.assertEqual(list(grid["se"][:4]), ["iid", "iid", "nw", "nw"])
        self.assertEqual(len(grid), 6)
        self.assertEqual(len(excluded), 2)
        self.assertTrue((excluded["reason"] == "Too short for the lag choice.").all())

    def test_time_series_means_and_uncertainty(self):
        data = series_data()
        manifest = freeze_manifest(series_menu(), data)
        results = run_manifest(manifest, data)
        self.assertTrue(validate_results(manifest, results)["passed"])
        a = data.loc[data["series"] == "a", "ret"].to_numpy()
        iid = results[(results["series"] == "a") & (results["se"] == "iid") & (results["window"] == "all")].iloc[0]
        self.assertAlmostEqual(iid["estimate"], a.mean(), places=12)
        self.assertAlmostEqual(iid["std_error"], a.std(ddof=1) / math.sqrt(len(a)), places=12)
        nw = results[(results["series"] == "a") & (results["se"] == "nw")].iloc[0]
        self.assertAlmostEqual(nw["std_error"], newey_west_se(a, 4), places=12)
        late = results[(results["series"] == "a") & (results["window"] == "late")].iloc[0]
        self.assertEqual(late["n_obs"], 60)

    def test_newey_west_zero_lags_is_iid_without_dof_correction(self):
        y = np.array([1.0, 2.0, 4.0, 3.0, 5.0])
        self.assertAlmostEqual(newey_west_se(y, 0), math.sqrt(np.mean((y - y.mean()) ** 2) / len(y)), places=12)

    def test_sample_hash_is_language_neutral(self):
        # Must equal R's sample_sha256(c(3, 1, 2, 100000)).
        self.assertEqual(sample_sha256(pd.Series([3.0, 1.0, 2.0, 100000.0])),
                         "9e8547d120531efa6d39d02233c788335a8968bf7bc39b3027ea4b1634862484")

    def test_menu_errors_are_explicit(self):
        data = series_data()
        conflict = series_menu()
        conflict["dimensions"][2]["options"][1]["vcov"] = {"type": "iid"}
        with self.assertRaisesRegex(MenuError, "Only one dimension may replace vcov"):
            freeze_manifest(conflict, data)
        duplicate = series_menu()
        duplicate["dimensions"][2]["options"][1]["filters"] = []
        with self.assertRaisesRegex(MenuError, "same model"):
            freeze_manifest(duplicate, data)
        outside = series_menu()
        outside["preferred"] = {"series": "a", "se": "nw", "window": "late", "sample_mode": "available"}
        with self.assertRaisesRegex(MenuError, "Preferred specification must be in the grid"):
            freeze_manifest(outside, data)

    def test_pooled_series_fail_with_reason(self):
        data = series_data()
        menu = series_menu()
        menu["dimensions"] = menu["dimensions"][1:2]
        menu["exclusions"] = []
        menu["preferred"] = {"se": "iid", "sample_mode": "available"}
        manifest = freeze_manifest(menu, data)
        results = run_manifest(manifest, data)
        self.assertTrue((results["status"] == "failed").all())
        self.assertTrue(results["error"].str.contains("exactly one series").all())


class EstimatorExtensionTests(unittest.TestCase):
    def test_iv_effective_f_and_weights(self):
        import pyfixest as pf

        rng = np.random.default_rng(7)
        n = 600
        data = pd.DataFrame({"row_id": np.arange(1, n + 1), "firm": np.repeat(np.arange(1, 61), 10),
                             "year": np.tile(np.arange(2001, 2011), 60)})
        data["z1"], data["z2"], data["c1"] = rng.normal(size=n), rng.normal(size=n), rng.normal(size=n)
        data["w"] = rng.uniform(0.5, 2, size=n)
        u = rng.normal(size=n)
        data["x"] = 0.8 * data["z1"] + 0.3 * data["z2"] + 0.5 * u + rng.normal(size=n) + data["firm"] / 60
        data["y"] = 1.5 * data["x"] + 0.4 * data["c1"] + u + data["year"] / 10
        menu = {"schema_version": 2, "version": "iv-test", "analysis_kind": "synthetic_demo", "estimator": "iv_fixed_effects",
                "title": "IV test", "estimand": "Effect of x on y", "row_id": "row_id", "unit": "firm", "time": "year",
                "base": {"outcome": "y", "treatment": "x", "instruments": ["z1", "z2"], "required_controls": ["c1"],
                         "fixed_effects": ["firm", "year"], "vcov": {"type": "cluster", "cluster": "firm"}},
                "confidence_level": 0.95,
                "dimensions": [
                    {"name": "instrument_set", "options": [
                        {"id": "both", "label": "Both"},
                        {"id": "z1", "label": "z1 only", "instruments": ["z1"], "add_controls": ["z2"]}]},
                    {"name": "weighting", "options": [
                        {"id": "none", "label": "Unweighted"}, {"id": "w", "label": "Weighted", "weights": "w"}]}],
                "sample_modes": ["available"], "preferred": {"instrument_set": "both", "weighting": "none", "sample_mode": "available"}}
        manifest = freeze_manifest(menu, data)
        results = run_manifest(manifest, data)
        assert validate_results(manifest, results)["passed"]
        ssc = pf.ssc(k_adj=True, k_fixef="nonnested", G_adj=True, G_df="min")
        direct = pf.feols("y ~ c1 | firm + year | x ~ z1 + z2", data=data, vcov={"CRV1": "firm"}, ssc=ssc, demeaner=pf.MapDemeaner(fixef_tol=1e-10))
        preferred = results[results["preferred"]].iloc[0]
        assert abs(preferred["estimate"] - direct.coef()["x"]) < 1e-10
        assert abs(preferred["std_error"] - direct.se()["x"]) < 1e-10
        first = pf.feols("x ~ z1 + z2 + c1 | firm + year", data=data, vcov={"CRV1": "firm"}, ssc=ssc, demeaner=pf.MapDemeaner(fixef_tol=1e-10))
        one = results[(results["instrument_set"] == "z1") & (results["weighting"] == "none")].iloc[0]
        assert abs(one["first_stage_f"] - first.tstat()["z1"] ** 2) < 1e-6
        weighted = results[(results["instrument_set"] == "both") & (results["weighting"] == "w")].iloc[0]
        wdirect = pf.feols("y ~ c1 | firm + year | x ~ z1 + z2", data=data, weights="w", vcov={"CRV1": "firm"}, ssc=ssc, demeaner=pf.MapDemeaner(fixef_tol=1e-10))
        assert abs(weighted["estimate"] - wdirect.coef()["x"]) < 1e-10 and weighted["weights"] == "w"
        assert (results["first_stage_f"] > 10).all()


def gmm_data():
    """Deterministic data shared with the R tests (paper/tests/test_curve.R)."""
    t = np.arange(1, 201, dtype=float)
    u = 0.5 * np.sin(2.71 * t + 0.5)
    data = pd.DataFrame({"row_id": t.astype(int), "series": "s", "time": t, "z1": np.sin(0.37 * t), "z2": np.cos(1.13 * t)})
    data["x"] = 1 + 0.8 * data["z1"] + 0.5 * data["z2"] + 0.5 * u + 0.3 * np.cos(0.91 * t)
    data["y"] = 0.5 + 2 * data["x"] + u
    return data, u


def gmm_menu():
    return {
        "schema_version": 2, "version": "gmm-test", "analysis_kind": "synthetic_demo", "estimator": "gmm",
        "title": "GMM test", "estimand": "Slope b in E[z (y - a - b x)] = 0", "row_id": "row_id", "unit": "series",
        "time": "time",
        "base": {"moments": {"name": "linear_iv", "r": "paper/tests/gmm_linear_moments.R",
                             "python": "python/tests/gmm_linear_moments.py"},
                 "parameters": [{"name": "a", "lower": -10, "upper": 10}, {"name": "b", "lower": -10, "upper": 10}],
                 "focal_parameter": "b", "moment_options": {"instruments": "both"}, "weighting": "two_step",
                 "search": {"type": "local"}, "start": {"a": 0, "b": 0}, "vcov": {"type": "newey_west", "lags": 0}},
        "confidence_level": 0.95,
        "dimensions": [
            {"name": "weighting", "label": "Weighting", "options": [
                {"id": "two_step", "label": "Two-step", "weighting": "two_step"},
                {"id": "identity", "label": "Identity", "weighting": "identity"},
                {"id": "iterated", "label": "Iterated", "weighting": "iterated"},
                {"id": "cue", "label": "CUE", "weighting": "cue"}]},
            {"name": "search", "label": "Search", "options": [
                {"id": "local", "label": "Local", "search": {"type": "local"}},
                {"id": "multi", "label": "Multistart", "search": {"type": "multistart", "starts": 3}}]},
            {"name": "z", "label": "Instruments", "options": [
                {"id": "both", "label": "z1 and z2"},
                {"id": "z1", "label": "z1 only", "moment_options": {"instruments": "z1"}}]},
            {"name": "hac", "label": "Long-run covariance", "options": [
                {"id": "nw0", "label": "None", "vcov": {"type": "newey_west", "lags": 0}},
                {"id": "varhac", "label": "VARHAC", "vcov": {"type": "varhac", "max_lag": 1, "criterion": "aic"}}]}],
        "exclusions": [{"match": {"weighting": ["cue"], "hac": ["varhac"]}, "reason": "CUE with VARHAC is not supported."}],
        "sample_modes": ["available"],
        "preferred": {"weighting": "two_step", "search": "local", "z": "both", "hac": "nw0", "sample_mode": "available"},
    }


class GmmTests(unittest.TestCase):
    def test_linear_gmm_matches_closed_forms(self):
        data, _ = gmm_data()
        menu = gmm_menu()
        manifest = freeze_manifest(menu, data)
        results = run_manifest(manifest, data)
        self.assertTrue(validate_results(manifest, results)["passed"])
        self.assertEqual(len(results), 28)
        self.assertTrue(all(c in results for c in gmm_row_fields(menu)))
        n = len(data)
        X = np.column_stack([np.ones(n), data["x"]])
        Z = np.column_stack([np.ones(n), data["z1"], data["z2"]])
        y = data["y"].to_numpy()

        def linear_gmm(W):
            return np.linalg.solve(X.T @ Z @ W @ Z.T @ X, X.T @ Z @ W @ Z.T @ y)

        theta1 = linear_gmm(np.eye(3))
        e1 = y - X @ theta1
        S1 = (e1[:, None] * Z).T @ (e1[:, None] * Z) / n
        theta2 = linear_gmm(np.linalg.inv(S1))
        e2 = y - X @ theta2
        S2 = (e2[:, None] * Z).T @ (e2[:, None] * Z) / n
        G = -Z.T @ X / n
        gbar = (e2[:, None] * Z).mean(axis=0)

        def pick(**choice):
            keep = np.ones(len(results), dtype=bool)
            for k, v in choice.items():
                keep &= (results[k] == v).to_numpy()
            return results[keep].iloc[0]

        one = pick(weighting="identity", search="local", z="both", hac="nw0")
        self.assertLess(abs(one["estimate"] - theta1[1]), 1e-8)
        self.assertTrue(math.isnan(one["j_stat"]))
        bread = np.linalg.inv(G.T @ G)
        self.assertLess(abs(one["std_error"] - math.sqrt((bread @ G.T @ S1 @ G @ bread / n)[1, 1])), 1e-7)
        two = pick(weighting="two_step", search="local", z="both", hac="nw0")
        self.assertLess(abs(two["estimate"] - theta2[1]), 1e-8)
        self.assertLess(abs(two["std_error"] - math.sqrt(np.linalg.inv(G.T @ np.linalg.solve(S2, G))[1, 1] / n)), 1e-7)
        self.assertLess(abs(two["j_stat"] - n * gbar @ np.linalg.solve(S2, gbar)), 1e-7)
        self.assertEqual(two["j_df"], 1)
        multi = pick(weighting="two_step", search="multi", z="both", hac="nw0")
        self.assertEqual(multi["n_starts"], 5)
        self.assertLess(abs(multi["estimate"] - theta2[1]), 1e-8)
        iterated = pick(weighting="iterated", search="local", z="both", hac="nw0")
        cue = pick(weighting="cue", search="multi", z="both", hac="nw0")
        cue_local = pick(weighting="cue", search="local", z="both", hac="nw0")
        self.assertGreaterEqual(iterated["weighting_updates"], 2)
        self.assertLessEqual(cue["j_stat"], two["j_stat"] + 1e-9)
        self.assertGreater(cue_local["j_stat"], cue["j_stat"])  # the CUE objective is multimodal
        just = pick(weighting="two_step", search="local", z="z1", hac="nw0")
        self.assertEqual(just["j_df"], 0)
        self.assertTrue(math.isnan(just["j_p_value"]))
        self.assertTrue((results.loc[results["hac"] == "varhac", "vcov"] == "VARHAC(1, AIC)").all())
        self.assertTrue(results["formula"].str.startswith("linear_iv(instruments=").all())

    def test_halton_and_varhac(self):
        self.assertTrue(np.allclose(halton_point(1, 3), [0.5, 1 / 3, 0.2]))
        self.assertTrue(np.allclose(halton_point(5, 2), [0.625, 7 / 9]))
        _, u = gmm_data()
        self.assertTrue(np.allclose(gmm_varhac(u[:, None], 0, "aic"), u @ u / len(u)))
        ar = np.zeros(len(u))
        for t in range(len(u)):
            ar[t] = u[t] + (0.6 * ar[t - 1] if t else 0.0)
        a_hat = ar[1:] @ ar[:-1] / (ar[:-1] @ ar[:-1])
        expected = np.mean((ar[1:] - a_hat * ar[:-1]) ** 2) / (1 - a_hat) ** 2
        self.assertLess(abs(gmm_varhac(ar[:, None], 1, "aic")[0, 0] - expected), 1e-12)

    def test_failures_bounds_and_unsupported_combinations(self):
        data, _ = gmm_data()
        broken = gmm_menu()
        broken["base"]["moment_options"]["broken"] = True
        broken["dimensions"] = broken["dimensions"][:1]
        broken["exclusions"] = []
        broken["preferred"] = {"weighting": "two_step", "sample_mode": "available"}
        broken_manifest = freeze_manifest(broken, data)
        results = run_manifest(broken_manifest, data)
        self.assertFalse(validate_results(broken_manifest, results)["passed"])
        broken_manifest["menu"]["allow_failed_rows"] = True
        with self.assertRaisesRegex(MenuError, "preferred specification failed"):
            validate_results(broken_manifest, results)
        self.assertTrue((results["status"] == "failed").all())
        self.assertTrue(results["error"].str.contains("objective not finite at the start").all())
        self.assertTrue((results["convergence"] == 3).all())
        bounded = gmm_menu()
        bounded["base"]["parameters"][1]["upper"] = 1.5
        bounded["dimensions"] = bounded["dimensions"][:1]
        bounded["exclusions"] = []
        bounded["preferred"] = {"weighting": "two_step", "sample_mode": "available"}
        results = run_manifest(freeze_manifest(bounded, data), data)
        self.assertTrue((results["status"] == "ok").all() and (results["bound_hits"] == "b").all())
        self.assertTrue((abs(results["estimate"] - 1.5) < 1e-12).all())
        unexcluded = gmm_menu()
        unexcluded["exclusions"] = []
        with self.assertRaisesRegex(MenuError, "CUE with VARHAC"):
            validate_menu(unexcluded, data)
        published = gmm_menu()
        published["dimensions"][1]["options"][0] = {"id": "local", "label": "Published", "search": {"type": "published"}}
        published["base"]["first_step"] = {"a": 0.4, "b": 2.1}
        with self.assertRaisesRegex(MenuError, "defines no estimator"):
            validate_menu(published, data)


if __name__ == "__main__":
    unittest.main()
