"""Moment functions for the durable-consumption model of Yogo (2006, JF).

Line-by-line twin of moments.R (see there for the parameters, moment blocks and
options); loaded by the gmm estimator family in python/speccurve/engine.py.
"""

from __future__ import annotations

import numpy as np
import pandas as pd

PARAMETERS = ["sigma", "gamma", "rho", "alpha", "beta"]


class MomentError(ValueError):
    pass


def _asset_columns(assets: str) -> list[str]:
    def block(prefix: str, k: int) -> list[str]:
        return [f"{prefix}_{j:02d}" for j in range(1, k + 1)]

    table = {"ff25": block("ff", 25), "ind24": block("ind", 24), "bet25": block("bet", 25),
             "all74": block("ff", 25) + block("ind", 24) + block("bet", 25), "ff3_conditional": block("fac", 3)}
    if assets not in table:
        raise MomentError(f"Unknown test-asset set: {assets}")
    return table[assets]


def _consumption(options: dict) -> list[str]:
    return [f"{name}_{options['timing']}" for name in ("gc", "dc", "ldc", "price", "lprice", "infl")]


def _conditional(options: dict) -> bool:
    return options.get("assets") == "ff3_conditional"


def _instruments(options: dict) -> list[str]:
    return [f"gc_{options['timing']}", f"gd_{options['timing']}"]


def variables(options: dict) -> list[str]:
    if options.get("timing") not in ("b", "e"):
        raise MomentError("Timing must be b (beginning of period) or e (end of period).")
    extra = _instruments(options) + ["z_dp", "z_size", "z_value", "z_yield"] if _conditional(options) else []
    return _consumption(options) + ["tbill", "market"] + _asset_columns(options["assets"]) + extra


def prepare(frame: pd.DataFrame, options: dict) -> dict:
    if not options.get("tbill") and not options.get("foc"):
        raise MomentError("Without the T-bill equation or the FOC the scale of the SDF, and so beta, is not identified.")
    c_vars = _consumption(options)
    lags = 2 if _conditional(options) else 0
    n = len(frame)
    if n <= lags + 10:
        raise MomentError("Too few quarters for the moment conditions.")
    now = np.arange(lags, n)

    def get(name: str) -> np.ndarray:
        return frame[name].to_numpy(dtype=float)[now]

    infl = get(c_vars[5])
    # The Fama-French factors are already excess returns; portfolios are not.
    assets = frame[_asset_columns(options["assets"])].to_numpy(dtype=float)[now]
    excess = (assets if _conditional(options) else assets - get("tbill")[:, None]) / infl[:, None]
    if lags:
        lagged = frame[_instruments(options) + ["z_dp", "z_size", "z_value", "z_yield"]].to_numpy(dtype=float)[: n - lags]
        z = np.column_stack([np.ones(n - lags), lagged])
    else:
        z = np.ones((len(now), 1))
    return {"gc": get(c_vars[0]), "dc": get(c_vars[1]), "ldc": get(c_vars[2]),
            "rprice": get(c_vars[3]) / get(c_vars[4]), "lprice": get(c_vars[4]),
            "rf": (1 + get("tbill")) / infl, "rw": (1 + get("market")) / infl, "excess": excess, "z": z,
            "tbill": bool(options.get("tbill")), "foc": bool(options.get("foc")),
            "depreciation": float(options.get("depreciation", 0.94)), "rows": now}


def moments(theta: np.ndarray, prep: dict) -> np.ndarray:
    sigma, gam, rho, alpha, beta = (float(x) for x in theta)
    with np.errstate(all="ignore"):
        kappa = (1 - gam) / (1 - 1 / sigma)
        v = (1 - alpha + alpha * prep["dc"] ** (1 - 1 / rho)) ** (1 / (1 - 1 / rho))
        lv = (1 - alpha + alpha * prep["ldc"] ** (1 - 1 / rho)) ** (1 / (1 - 1 / rho))
        m = (beta ** kappa * np.exp(prep["gc"]) ** (-kappa / sigma) * (v / lv) ** (kappa * (1 / rho - 1 / sigma))
             * prep["rw"] ** (kappa - 1))
        blocks = []
        if prep["tbill"]:
            blocks.append((m * prep["rf"] - 1)[:, None])
        blocks.append(m[:, None] * prep["excess"])
        if prep["foc"]:
            foc = 1 - alpha / (1 - alpha) * prep["ldc"] ** (-1 / rho) / prep["lprice"] - prep["depreciation"] * m * prep["rprice"]
            blocks.append(foc[:, None])
        errors = np.column_stack(blocks)
        z = prep["z"]
        if z.shape[1] == 1:
            return errors
        # GAUSS's horizontal direct product: each error times every instrument.
        return np.column_stack([errors[:, [j]] * z for j in range(errors.shape[1])])


gmm_model = {"name": "yogo_durable", "parameters": PARAMETERS, "variables": variables, "prepare": prepare,
             "moments": moments}
