"""Unit tests for the Python port (run: python -m unittest discover -s python/tests)."""

import math
import sys
import unittest
from pathlib import Path

import numpy as np
import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from speccurve.engine import (MenuError, choice_grid, freeze_manifest, newey_west_se, run_manifest,  # noqa: E402
                              sample_sha256, validate_results)


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


if __name__ == "__main__":
    unittest.main()
