"""Python port of the reference specification-curve workflow (menu schema 2).

The R implementation in ``paper/R`` is the reference. This module reproduces
its grid order, specification identifiers, filters, samples, estimators
(weighted fixed-effects regressions, instrumental variables with fixed effects,
time-series means) and uncertainty conventions so the two can be compared row by
row. It provides no joint inference.
"""

from __future__ import annotations

import hashlib
import itertools
import json
import math
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

import numpy as np
import pandas as pd
from scipy import stats

ESTIMATORS = ("linear_fixed_effects", "iv_fixed_effects", "time_series_mean")
FILTER_OPS = ("==", "!=", ">=", "<=", ">", "<", "in", "not_in")
REPLACEMENT_FIELDS = ("outcome", "treatment", "instruments", "fixed_effects", "unit_trend", "vcov", "weights")
RESULT_FIELDS = (
    "panel", "formula", "estimator", "outcome", "treatment", "control_vars", "fixed_effect_terms",
    "unit_trend", "vcov", "filters", "confidence_level", "status", "error", "estimate", "std_error",
    "statistic", "p_value", "ci_lower", "ci_upper", "n_input", "n_eligible", "n_obs", "n_units",
    "n_periods", "n_clusters", "n_estimator_dropped", "sample_hash", "dropped_terms",
    "manifest_hash", "input_hash", "facet", "rank", "role", "instruments", "weights", "first_stage_f",
)
RESERVED = ("spec_id", "sample_mode", "preferred", "original") + RESULT_FIELDS


class MenuError(ValueError):
    """Raised when a menu, manifest or result set violates the workflow contract."""


def abort_if(condition: bool, message: str) -> None:
    if condition:
        raise MenuError(message)


def read_menu(path: str | Path) -> dict[str, Any]:
    return json.loads(Path(path).read_text())


def file_sha256(path: str | Path) -> str:
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def frame_sha256(data: pd.DataFrame) -> str:
    """Language-neutral input fingerprint: canonical CSV of sorted columns and rows."""
    ordered = data.reindex(sorted(data.columns), axis=1)
    text = ordered.to_csv(index=False, float_format="%.10g", lineterminator="\n")
    return hashlib.sha256(text.encode("utf-8")).hexdigest()


def sample_sha256(row_ids: pd.Series) -> str:
    """Hash of the sorted observation identifiers, as text, one per line."""
    text = "\n".join(sorted(str_id(v) for v in row_ids))
    return hashlib.sha256(text.encode("utf-8")).hexdigest()


def str_id(value: Any) -> str:
    if isinstance(value, (float, np.floating)) and float(value).is_integer():
        return str(int(value))
    return str(value)


@dataclass
class Spec:
    outcome: str
    treatment: str | None
    controls: list[str]
    fixed_effects: list[str]
    unit_trend: bool
    vcov: dict[str, Any]
    filters: list[dict[str, Any]] = field(default_factory=list)
    panel: str = "All specifications"
    instruments: list[str] = field(default_factory=list)
    weights: str | None = None

    @property
    def filter_signature(self) -> str:
        if not self.filters:
            return "none"
        return " & ".join(f"{f['variable']} {f['op']} {format_value(f['value'])}" for f in self.filters)


def format_value(value: Any) -> str:
    values = value if isinstance(value, list) else [value]
    return ",".join(format_scalar(v) for v in values)


def format_scalar(value: Any) -> str:
    # Match R's paste() of numbers: integers print without decimals.
    if isinstance(value, bool):
        return "TRUE" if value else "FALSE"
    if isinstance(value, float) and value.is_integer():
        return str(int(value))
    return str(value)


def vcov_label(vcov: dict[str, Any]) -> str:
    kind = vcov["type"]
    if kind == "cluster":
        return f"cluster({vcov['cluster']})"
    if kind == "iid":
        return "iid"
    if kind == "hetero":
        return "heteroskedasticity-robust"
    if kind == "newey_west":
        return f"Newey-West({format_scalar(vcov['lags'])})"
    raise MenuError("Unknown vcov type.")


def find_option(menu: dict[str, Any], dimension: str, option_id: str) -> dict[str, Any]:
    for dim in menu["dimensions"]:
        if dim["name"] == dimension:
            for option in dim["options"]:
                if option["id"] == option_id:
                    return option
    raise MenuError(f"Unknown option {dimension}={option_id}")


def resolve_spec(menu: dict[str, Any], choice: dict[str, str]) -> Spec:
    base = menu["base"]
    spec = Spec(
        outcome=base.get("outcome"), treatment=base.get("treatment"),
        controls=list(base.get("required_controls") or []),
        fixed_effects=list(base.get("fixed_effects") or []),
        unit_trend=bool(base.get("unit_trend", False)), vcov=dict(base.get("vcov") or {}),
        filters=[dict(f) for f in base.get("filters") or []],
        instruments=list(base.get("instruments") or []), weights=base.get("weights"),
    )
    for dim in menu["dimensions"]:
        option = find_option(menu, dim["name"], choice[dim["name"]])
        for name in REPLACEMENT_FIELDS:
            if option.get(name) is not None:
                value = option[name]
                setattr(spec, name, list(value) if name in ("fixed_effects", "instruments") else
                        dict(value) if name == "vcov" else value)
        for control in option.get("add_controls") or []:
            if control not in spec.controls:
                spec.controls.append(control)
        spec.filters.extend(dict(f) for f in option.get("filters") or [])
        if dim["name"] == menu.get("panel_by"):
            spec.panel = option["label"]
    return spec


def spec_variables(menu: dict[str, Any], spec: Spec) -> list[str]:
    names = [spec.outcome, spec.treatment, *spec.instruments, *spec.controls, *spec.fixed_effects, spec.weights]
    if spec.vcov.get("type") == "cluster":
        names.append(spec.vcov["cluster"])
    names += [f["variable"] for f in spec.filters]
    names += [menu["unit"], menu["time"], menu["row_id"]]
    out: list[str] = []
    for name in names:
        if name is not None and name not in out:
            out.append(name)
    return out


def weights_label(spec: Spec) -> str:
    return spec.weights if spec.weights else "none"


def instruments_label(spec: Spec) -> str:
    return ";".join(spec.instruments)


def model_formula(menu: dict[str, Any], spec: Spec) -> str:
    if menu["estimator"] == "time_series_mean":
        return f"{spec.outcome} ~ 1"
    iv = menu["estimator"] == "iv_fixed_effects"
    rhs = list(spec.controls) if iv else [spec.treatment, *spec.controls]
    formula = f"{spec.outcome} ~ " + (" + ".join(rhs) if rhs else "1")
    fixed = [f"{menu['unit']}[{menu['time']}]" if (spec.unit_trend and fe == menu["unit"]) else fe
             for fe in spec.fixed_effects]
    if fixed:
        formula += " | " + " + ".join(fixed)
    if iv:
        formula += f" | {spec.treatment} ~ " + " + ".join(spec.instruments)
    return formula


def choice_grid(menu: dict[str, Any]) -> tuple[pd.DataFrame, pd.DataFrame]:
    """Cartesian grid in R expand.grid order: the first dimension varies fastest."""
    levels = {dim["name"]: [o["id"] for o in dim["options"]] for dim in menu["dimensions"]}
    levels["sample_mode"] = list(menu["sample_modes"])
    names = list(levels)
    rows = [dict(zip(names, reversed(combo))) for combo in itertools.product(*(levels[n] for n in reversed(names)))]
    grid = pd.DataFrame(rows, columns=names)
    excluded = pd.Series(False, index=grid.index)
    reasons = pd.Series("", index=grid.index)
    for rule in menu.get("exclusions") or []:
        hit = pd.Series(True, index=grid.index)
        for name, value in rule["match"].items():
            allowed = value if isinstance(value, list) else [value]
            hit &= grid[name].isin(allowed)
        hit &= ~excluded
        reasons[hit] = rule["reason"]
        excluded |= hit
    dropped = grid[excluded].copy()
    dropped["reason"] = reasons[excluded]
    return grid[~excluded].reset_index(drop=True), dropped.reset_index(drop=True)


def same_choice(grid: pd.DataFrame, choice: dict[str, str]) -> pd.Series:
    keep = pd.Series(True, index=grid.index)
    for name, value in choice.items():
        keep &= grid[name] == value
    return keep


def validate_vcov(menu: dict[str, Any], vcov: dict[str, Any]) -> None:
    abort_if(not vcov.get("type"), "Every specification needs an uncertainty convention.")
    allowed = ("iid", "newey_west") if menu["estimator"] == "time_series_mean" else ("cluster", "iid", "hetero")
    abort_if(vcov["type"] not in allowed, f"Unsupported vcov for {menu['estimator']} : {vcov['type']}")
    if vcov["type"] == "cluster":
        abort_if(not vcov.get("cluster"), "Cluster variable missing.")
    if vcov["type"] == "newey_west":
        lags = vcov.get("lags")
        abort_if(not isinstance(lags, (int, float)) or lags < 0 or lags != round(lags),
                 "Newey-West lags must be a nonnegative integer.")


def validate_menu(menu: dict[str, Any], data: pd.DataFrame) -> tuple[pd.DataFrame, pd.DataFrame]:
    required = ("schema_version", "version", "analysis_kind", "estimator", "title", "estimand", "row_id",
                "unit", "time", "base", "confidence_level", "dimensions", "sample_modes", "preferred")
    missing = [f for f in required if f not in menu]
    abort_if(bool(missing), "Menu is missing required fields: " + ", ".join(missing))
    abort_if(menu["schema_version"] != 2, "Menu schema_version must be 2.")
    abort_if(menu["analysis_kind"] not in ("synthetic_demo", "application"), "Unknown analysis kind.")
    abort_if(menu["estimator"] not in ESTIMATORS, "Supported estimators: " + ", ".join(ESTIMATORS))
    dims = menu["dimensions"]
    abort_if(not dims, "Specify at least one choice dimension.")
    names = [d["name"] for d in dims]
    abort_if(len(set(names)) != len(names) or any(n in RESERVED for n in names)
             or any(not n[:1].isalpha() or not n.replace("_", "").isalnum() for n in names),
             "Dimension names must be unique simple identifiers and not reserved.")
    for dim in dims:
        ids = [o["id"] for o in dim["options"]]
        abort_if(not ids or len(set(ids)) != len(ids), f"Options must be unique and nonempty: {dim['name']}")
        abort_if(any(not o.get("label") for o in dim["options"]), f"Every option needs a label: {dim['name']}")
    for name in REPLACEMENT_FIELDS:
        setters = sum(any(o.get(name) is not None for o in d["options"]) for d in dims)
        abort_if(setters > 1, f"Only one dimension may replace {name}")
    if menu.get("panel_by") is not None:
        abort_if(menu["panel_by"] not in names, "panel_by must name a dimension.")
    modes = list(menu["sample_modes"])
    abort_if(not modes or len(set(modes)) != len(modes) or any(m not in ("available", "common") for m in modes),
             "Sample modes must be unique available/common choices.")
    if menu["estimator"] == "time_series_mean":
        abort_if(modes != ["available"], "Time-series means use available samples only.")
    level = menu["confidence_level"]
    abort_if(not isinstance(level, (int, float)) or not 0 < level < 1, "Confidence level must be between zero and one.")
    ids = data[menu["row_id"]]
    abort_if(ids.isna().any() or ids.duplicated().any(), "Observation identifiers must be unique and nonmissing.")
    for rule in menu.get("exclusions") or []:
        abort_if(not rule.get("reason"), "Every exclusion needs a reason.")
        abort_if(any(k not in names + ["sample_mode"] for k in rule["match"]), "Exclusions must reference dimensions.")
    grid, excluded = choice_grid(menu)
    abort_if(grid.empty, "The menu excludes every specification.")
    preferred = menu["preferred"]
    abort_if(set(preferred) != set(names + ["sample_mode"]),
             "Preferred specification must choose every dimension and the sample mode.")
    abort_if(same_choice(grid, preferred).sum() != 1, "Preferred specification must be in the grid.")
    for choice in menu.get("original_specifications") or []:
        abort_if(set(choice) != set(names + ["sample_mode"]) or same_choice(grid, choice).sum() != 1,
                 "Every original specification must identify one grid row.")
    signatures = []
    for _, row in grid.iterrows():
        spec = resolve_spec(menu, row.to_dict())
        validate_vcov(menu, spec.vcov)
        regression = menu["estimator"] != "time_series_mean"
        abort_if(spec.outcome is None or (regression and spec.treatment is None),
                 "Every specification needs an outcome and, for regressions, a focal variable.")
        abort_if(spec.outcome in spec.controls + [spec.treatment] + spec.instruments,
                 "The outcome cannot also be a regressor.")
        if menu["estimator"] == "iv_fixed_effects":
            abort_if(not spec.instruments, "Instrumental-variables specifications need at least one instrument.")
            abort_if(any(z in spec.controls + [spec.treatment] for z in spec.instruments)
                     or len(set(spec.instruments)) != len(spec.instruments),
                     "Instruments must be distinct from the endogenous variable and the controls.")
        else:
            abort_if(bool(spec.instruments), "Instruments are only used by iv_fixed_effects.")
        abort_if(spec.weights is not None and not regression, "Weights are only used by regression estimators.")
        if spec.unit_trend:
            abort_if(menu["unit"] not in spec.fixed_effects, "Unit trends require unit fixed effects.")
        variables = spec_variables(menu, spec)
        absent = [v for v in variables if v not in data.columns]
        abort_if(bool(absent), "Menu references variables absent from input data: " + ", ".join(absent))
        abort_if(not pd.api.types.is_numeric_dtype(data[spec.outcome]) or
                 (spec.treatment is not None and not pd.api.types.is_numeric_dtype(data[spec.treatment])) or
                 any(not pd.api.types.is_numeric_dtype(data[z]) for z in spec.instruments),
                 "Outcome, focal, and instrument variables must be numeric.")
        if spec.weights is not None:
            abort_if(not pd.api.types.is_numeric_dtype(data[spec.weights]) or bool((data[spec.weights] <= 0).any()),
                     "Weights must be positive numbers.")
        for f in spec.filters:
            abort_if(f.get("op") not in FILTER_OPS or f.get("value") is None, "Unsupported filter.")
        signatures.append(" | ".join([model_formula(menu, spec), vcov_label(spec.vcov), weights_label(spec),
                                      spec.filter_signature, row["sample_mode"]]))
    abort_if(len(set(signatures)) != len(signatures) and not menu.get("allow_duplicate_models"),
             "Two grid rows define the same model and sample; remove the redundancy or declare it.")
    if menu["analysis_kind"] == "application":
        ref = menu.get("reference") or {}
        abort_if(not isinstance(ref.get("estimate"), (int, float)) or not isinstance(ref.get("tolerance"), (int, float))
                 or ref.get("tolerance", -1) < 0 or not ref.get("source"),
                 "Applications require a published baseline estimate, tolerance, and source.")
    return grid, excluded


def freeze_manifest(menu: dict[str, Any], data: pd.DataFrame) -> dict[str, Any]:
    """Build the frozen grid. The R manifest file remains the canonical record."""
    grid, excluded = validate_menu(menu, data)
    grid = grid.copy()
    grid["spec_id"] = [f"S{i + 1:04d}" for i in range(len(grid))]
    grid["preferred"] = same_choice(grid, menu["preferred"])
    grid["original"] = grid["preferred"].copy()
    for choice in menu.get("original_specifications") or []:
        grid["original"] |= same_choice(grid, choice)
    return {"schema_version": 2, "menu": menu, "expected_rows": len(grid), "grid": grid,
            "excluded": excluded, "input_hash": frame_sha256(data)}


def apply_filters(data: pd.DataFrame, filters: list[dict[str, Any]]) -> pd.Series:
    keep = pd.Series(True, index=data.index)
    for f in filters:
        x = data[f["variable"]]
        value = f["value"]
        op = f["op"]
        if op == "in":
            hit = x.isin(value if isinstance(value, list) else [value])
        elif op == "not_in":
            hit = ~x.isin(value if isinstance(value, list) else [value])
        else:
            hit = {"==": x == value, "!=": x != value, ">=": x >= value, "<=": x <= value,
                   ">": x > value, "<": x < value}[op]
        keep &= hit.fillna(False).astype(bool) & x.notna()
    return keep


def newey_west_se(y: np.ndarray, lags: int) -> float:
    n = len(y)
    u = y - y.mean()
    s = float(np.sum(u * u) / n)
    for lag in range(1, min(lags, n - 1) + 1):
        s += 2 * (1 - lag / (lags + 1)) * float(np.sum(u[lag:] * u[:-lag]) / n)
    return math.sqrt(s / n)


def estimate_time_series_mean(menu: dict[str, Any], spec: Spec, frame: pd.DataFrame) -> dict[str, Any]:
    abort_if(frame[menu["unit"]].nunique() != 1, "A time-series mean needs exactly one series.")
    abort_if(frame[menu["time"]].duplicated().any(), "Duplicate periods in a time series.")
    frame = frame.sort_values(menu["time"], kind="mergesort")
    y = frame[spec.outcome].to_numpy(dtype=float)
    n = len(y)
    abort_if(n < 3, "Fewer than three periods.")
    se = float(np.std(y, ddof=1) / math.sqrt(n)) if spec.vcov["type"] == "iid" else newey_west_se(y, int(spec.vcov["lags"]))
    estimate = float(y.mean())
    statistic = estimate / se
    critical = stats.t.ppf(1 - (1 - menu["confidence_level"]) / 2, df=n - 1)
    return {"estimate": estimate, "std_error": se, "statistic": statistic,
            "p_value": float(2 * stats.t.cdf(-abs(statistic), df=n - 1)),
            "ci_lower": estimate - critical * se, "ci_upper": estimate + critical * se,
            "n_obs": n, "used": frame, "dropped_terms": "", "n_clusters": None}


def pyfixest_vcov(vcov: dict[str, Any]) -> Any:
    kind = vcov["type"]
    return {"CRV1": vcov["cluster"]} if kind == "cluster" else ("iid" if kind == "iid" else "hetero")


def fit_pyfixest(formula: str, frame: pd.DataFrame, spec: Spec, vcov: Any = None, fixef_rm: str = "singleton") -> Any:
    import warnings

    import pyfixest as pf  # Imported lazily so time-series users need not install it.

    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        return pf.feols(formula, data=frame, vcov=pyfixest_vcov(spec.vcov) if vcov is None else vcov,
                        weights=spec.weights, fixef_rm=fixef_rm, demeaner=pf.MapDemeaner(fixef_tol=1e-10),
                        ssc=pf.ssc(k_adj=True, k_fixef="nonnested", G_adj=True, G_df="min"))


def effective_first_stage_f(spec: Spec, used: pd.DataFrame) -> float:
    """Montiel Olea-Pflueger effective F for one endogenous regressor (see the R engine)."""
    fixed = (" | " + " + ".join(spec.fixed_effects)) if spec.fixed_effects else ""
    used = used.reset_index(drop=True)
    first = fit_pyfixest(f"{spec.treatment} ~ " + " + ".join([*spec.instruments, *spec.controls]) + fixed, used, spec)
    coefs = first.coef()
    abort_if(int(first._N) != len(used) or any(z not in coefs.index for z in spec.instruments),
             "First stage is not identified on the estimation sample.")
    names = list(coefs.index)
    index = [names.index(z) for z in spec.instruments]
    pi = coefs[spec.instruments].to_numpy(dtype=float)
    v = np.asarray(first._vcov)[np.ix_(index, index)]
    rhs = " + ".join(spec.controls) if spec.controls else "1"
    partial = np.column_stack([
        np.asarray(fit_pyfixest(f"{z} ~ {rhs}{fixed}", used, spec, vcov="iid", fixef_rm="none").resid(), dtype=float)
        for z in spec.instruments])
    abort_if(partial.shape[0] != len(used), "First stage is not identified on the estimation sample.")
    w = used[spec.weights].to_numpy(dtype=float) if spec.weights else np.ones(len(used))
    zz = (partial * w[:, None]).T @ partial
    return float(pi @ zz @ pi / np.trace(v @ zz))


def estimate_fixed_effects(menu: dict[str, Any], spec: Spec, frame: pd.DataFrame, formula: str) -> dict[str, Any]:
    import warnings

    import pyfixest as pf  # Imported lazily so time-series users need not install it.

    kind = spec.vcov["type"]
    vcov: Any = pyfixest_vcov(spec.vcov)
    iv = menu["estimator"] == "iv_fixed_effects"
    frame = frame.reset_index(drop=True)
    fit_formula = formula
    slope_prefix = None
    if spec.unit_trend:
        # pyfixest has no varying-slope fixed effects. Unit-specific linear trends
        # enter as explicit unit-by-time regressors, which give the same focal
        # coefficient; the standard error is then put on fixest's parameter count
        # (all unit slopes, including the one made redundant by time effects).
        frame = frame.assign(_unit_trend_time=frame[menu["time"]].astype(float))
        unit, time = menu["unit"], menu["time"]
        rhs, fixed = formula.split(" | ")
        fit_formula = f"{rhs} + i({unit}, _unit_trend_time) | " + fixed.replace(f"{unit}[{time}]", unit)
        slope_prefix = f"{unit}::"
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        # fixest drops fixed-effect singletons by default; match it.
        model = pf.feols(fit_formula, data=frame, vcov=vcov, weights=spec.weights, fixef_rm="singleton", demeaner=pf.MapDemeaner(fixef_tol=1e-10),
                         ssc=pf.ssc(k_adj=True, k_fixef="nonnested", G_adj=True, G_df="min"))
    coefs = model.coef()
    abort_if(spec.treatment not in coefs.index, "Focal coefficient is unidentified or absorbed.")
    collinear = list(getattr(model, "_collin_vars", []) or [])
    dropped = [c for c in collinear if not (slope_prefix and c.startswith(slope_prefix))]
    required = list(menu["base"].get("required_controls") or [])
    abort_if(any(c in dropped for c in required), "A required control is unidentified or absorbed.")
    level = menu["confidence_level"]
    estimate = float(coefs[spec.treatment])
    se = float(model.se()[spec.treatment])
    df = float(model._df_t)
    if spec.unit_trend:
        # fixest counts every unit slope; pyfixest drops the one that is collinear
        # with the time effects. Restore fixest's count in the small-sample factor
        # and, for non-clustered errors, in the t degrees of freedom.
        redundant = len(collinear) - len(dropped)
        n, k = float(model._N), float(model._df_k)
        se *= math.sqrt((n - k) / (n - k - redundant))
        if kind != "cluster":
            df -= redundant
    statistic = estimate / se
    critical = stats.t.ppf(1 - (1 - level) / 2, df=df)
    used_index = getattr(model, "_data", None)
    used = frame.loc[used_index.index] if used_index is not None else frame
    return {"estimate": estimate, "std_error": se, "statistic": statistic,
            "p_value": float(2 * stats.t.sf(abs(statistic), df=df)),
            "ci_lower": estimate - critical * se, "ci_upper": estimate + critical * se,
            "n_obs": int(model._N), "used": used, "dropped_terms": ";".join(dropped),
            "n_clusters": int(used[spec.vcov["cluster"]].nunique()) if kind == "cluster" else None,
            "first_stage_f": effective_first_stage_f(spec, used) if iv else None}


def run_manifest(manifest: dict[str, Any], data: pd.DataFrame) -> pd.DataFrame:
    menu = manifest["menu"]
    grid = manifest["grid"]
    specs = [resolve_spec(menu, row.to_dict()) for _, row in grid.iterrows()]
    common_vars: list[str] = []
    for spec in specs:
        for name in spec_variables(menu, spec):
            if name not in common_vars:
                common_vars.append(name)
    common = data[common_vars].notna().all(axis=1)
    rows = []
    for i, spec in enumerate(specs):
        choice = grid.iloc[i].to_dict()
        variables = spec_variables(menu, spec)
        complete = common if choice["sample_mode"] == "common" else data[variables].notna().all(axis=1)
        frame = data[complete & apply_filters(data, spec.filters)]
        formula = model_formula(menu, spec)
        row = dict(choice)
        row.update(panel=spec.panel, formula=formula, estimator=menu["estimator"], outcome=spec.outcome,
                   treatment=spec.treatment or "(intercept)", control_vars=";".join(spec.controls),
                   fixed_effect_terms=";".join(spec.fixed_effects), unit_trend=spec.unit_trend,
                   vcov=vcov_label(spec.vcov), filters=spec.filter_signature,
                   instruments=instruments_label(spec), weights=weights_label(spec), first_stage_f=np.nan,
                   confidence_level=menu["confidence_level"], status="failed", error="",
                   estimate=np.nan, std_error=np.nan, statistic=np.nan, p_value=np.nan, ci_lower=np.nan,
                   ci_upper=np.nan, n_input=len(data), n_eligible=len(frame), n_obs=np.nan, n_units=np.nan,
                   n_periods=np.nan, n_clusters=np.nan, n_estimator_dropped=np.nan, sample_hash="",
                   dropped_terms="", input_hash=manifest["input_hash"])
        try:
            abort_if(frame.empty, "No complete observations.")
            if menu["estimator"] == "time_series_mean":
                fit = estimate_time_series_mean(menu, spec, frame)
            else:
                fit = estimate_fixed_effects(menu, spec, frame, formula)
            for name in ("estimate", "std_error", "statistic", "p_value", "ci_lower", "ci_upper", "n_obs",
                         "dropped_terms"):
                row[name] = fit[name]
            row["n_clusters"] = np.nan if fit["n_clusters"] is None else fit["n_clusters"]
            if fit.get("first_stage_f") is not None:
                row["first_stage_f"] = fit["first_stage_f"]
            used = fit["used"]
            row["n_units"] = used[menu["unit"]].nunique()
            row["n_periods"] = used[menu["time"]].nunique()
            row["n_estimator_dropped"] = len(frame) - row["n_obs"]
            row["sample_hash"] = sample_sha256(used[menu["row_id"]])
            values = [row[k] for k in ("estimate", "std_error", "p_value", "ci_lower", "ci_upper")]
            abort_if(not all(np.isfinite(values)), "Uncertainty is not finite.")
            abort_if(spec.vcov["type"] == "cluster" and row["n_clusters"] < 2, "Fewer than two clusters remain.")
            abort_if(menu["estimator"] == "iv_fixed_effects" and not np.isfinite(row["first_stage_f"]),
                     "First-stage F is not finite.")
            row["status"] = "ok"
        except Exception as error:  # Every failure is retained as a row with its reason.
            row["error"] = str(error)
        rows.append(row)
    return pd.DataFrame(rows)


def validate_results(manifest: dict[str, Any], results: pd.DataFrame) -> dict[str, Any]:
    grid = manifest["grid"]
    menu = manifest["menu"]
    abort_if(results["spec_id"].duplicated().any() or len(results) != manifest["expected_rows"]
             or set(results["spec_id"]) != set(grid["spec_id"]),
             "Results must contain exactly one row for every manifest ID.")
    results = results.set_index("spec_id").loc[grid["spec_id"]].reset_index()
    for name in grid.columns:
        abort_if(list(results[name]) != list(grid[name]), f"Specification labels changed: {name}")
    for i, row in grid.iterrows():
        spec = resolve_spec(menu, row.to_dict())
        got = results.iloc[i]
        abort_if(got["outcome"] != spec.outcome or got["vcov"] != vcov_label(spec.vcov)
                 or got["formula"] != model_formula(menu, spec) or got["filters"] != spec.filter_signature
                 or ("weights" in results and got["weights"] != weights_label(spec))
                 or ("instruments" in results and (got["instruments"] if isinstance(got["instruments"], str) else "")
                     != instruments_label(spec)),
                 f"Result convention changed for {row['spec_id']}")
    failed = results["status"] == "failed"
    abort_if((failed & (results["error"] == "")).any(), "Failed specifications need an explicit reason.")
    ok = results[~failed]
    abort_if(((ok["ci_lower"] > ok["estimate"]) | (ok["ci_upper"] < ok["estimate"]) | (ok["std_error"] < 0)).any(),
             "Invalid uncertainty or sample size.")
    common = ok[ok["sample_mode"] == "common"]
    for _, group in common.groupby("filters"):
        abort_if(group["sample_hash"].nunique() > 1, "Common-sample specifications used different observations.")
    preferred = results[results["preferred"]].iloc[0]
    baseline = "not_applicable_synthetic_demo"
    reference = menu.get("reference")
    if reference:
        abort_if(preferred["status"] != "ok" or abs(preferred["estimate"] - reference["estimate"]) > reference["tolerance"],
                 "Preferred specification does not reproduce the documented published baseline.")
        baseline = "passed"
    return {"passed": not failed.any(), "expected_rows": manifest["expected_rows"],
            "completed_rows": int((~failed).sum()), "failed_ids": list(results.loc[failed, "spec_id"]),
            "baseline_reproduction": baseline, "joint_inference": "not_implemented"}


def summarise_results(manifest: dict[str, Any], results: pd.DataFrame) -> pd.DataFrame:
    alpha = 1 - manifest["menu"]["confidence_level"]
    ok = results[results["status"] == "ok"]
    rows = []
    for (panel, mode), group in ok.groupby(["panel", "sample_mode"], sort=True):
        baseline = group.loc[group["preferred"], "estimate"]
        rows.append({
            "panel": panel, "sample_mode": mode, "specifications": len(group),
            "median_estimate": float(group["estimate"].median()), "minimum_estimate": float(group["estimate"].min()),
            "maximum_estimate": float(group["estimate"].max()), "fraction_positive": float((group["estimate"] > 0).mean()),
            "fraction_significant_positive": float(((group["p_value"] < alpha) & (group["estimate"] > 0)).mean()),
            "fraction_significant_negative": float(((group["p_value"] < alpha) & (group["estimate"] < 0)).mean()),
            "preferred_estimate": float(baseline.iloc[0]) if len(baseline) else np.nan,
            "preferred_percentile": float((group["estimate"] <= baseline.iloc[0]).mean()) if len(baseline) else np.nan,
        })
    return pd.DataFrame(rows)
