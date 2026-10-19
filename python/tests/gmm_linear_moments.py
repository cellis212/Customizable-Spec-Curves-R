"""Linear moment model for the gmm tests (twin of paper/tests/gmm_linear_moments.R)."""

import numpy as np


def _instruments(options):
    return ["z1"] if options.get("instruments") == "z1" else ["z1", "z2"]


def variables(options):
    return ["y", "x"] + _instruments(options)


def prepare(frame, options):
    z = np.column_stack([np.ones(len(frame)), frame[_instruments(options)].to_numpy(dtype=float)])
    return {"y": frame["y"].to_numpy(dtype=float), "x": frame["x"].to_numpy(dtype=float), "z": z,
            "broken": bool(options.get("broken")), "rows": np.arange(len(frame))}


def moments(theta, prep):
    e = prep["y"] - theta[0] - theta[1] * prep["x"]
    if prep["broken"]:
        e = e * np.nan
    return e[:, None] * prep["z"]


gmm_model = {"name": "linear_iv", "parameters": ["a", "b"], "variables": variables, "prepare": prepare,
             "moments": moments}
