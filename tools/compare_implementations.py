"""Compare two results.csv files from different implementations, row by row.

Usage: python3 tools/compare_implementations.py REFERENCE.csv CANDIDATE.csv [--rtol 1e-8]

Checks identical specification IDs, sample modes, preferred/original flags, panel
labels, model formulas, outcomes, uncertainty labels, filters, status, samples
(observation hashes and counts), and numerically close estimates, standard errors, intervals, and IV first-stage F (p-values to at
least 1e-6 relative, because libraries compute t tails differently; estimates and interval bounds
relative to the larger of their value and their standard error). For GMM rows it also checks the
convergence code, bound hits, moment and start counts, the objective, J statistic and p-value, and
every parameter (on the scale of its standard error) and standard error.

Nonlinear searches (the gmm estimator) need not end at the same point to machine
precision: tiny floating-point differences can move a stopping point along a flat
objective, change which local optimum a search reaches, or flip a convergence test.
For them, --summary reports agreement instead of requiring it row by row: the share
of rows with the same status, and, among rows that succeed in both, the share whose
estimates agree within --close-tol standard errors. It fails only if those shares
fall below --min-status and --min-close.
Uses only the standard library so it runs wherever the implementations do.
"""

import argparse
import csv
import hashlib
import math
import os
import sys

KEYS = ("spec_id", "sample_mode", "preferred", "original", "panel", "formula", "outcome", "vcov", "filters", "status",
        "instruments", "weights", "bound_hits")
COUNTS = ("n_eligible", "n_obs", "n_units", "n_periods", "n_clusters", "convergence", "n_moments", "j_df", "n_starts")
NUMBERS = ("estimate", "std_error", "statistic", "p_value", "ci_lower", "ci_upper", "first_stage_f",
           "objective", "j_stat", "j_p_value")
BOOLS = {"TRUE": "true", "True": "true", "FALSE": "false", "False": "false"}


def load(path):
    with open(path, newline="") as handle:
        rows = {row["spec_id"]: row for row in csv.DictReader(handle)}
    # Stata writes the observations used to samples.csv instead of hashing them.
    samples = os.path.join(os.path.dirname(path), "samples.csv")
    if rows and "sample_hash" not in next(iter(rows.values())) and os.path.exists(samples):
        ids = {}
        with open(samples, newline="") as handle:
            for row in csv.DictReader(handle):
                ids.setdefault(row["spec_id"], []).append(row["row_id"].strip())
        for spec_id, row in rows.items():
            text = "\n".join(sorted(ids.get(spec_id, [])))
            row["sample_hash"] = hashlib.sha256(text.encode("utf-8")).hexdigest()
    return rows


def norm(value):
    return BOOLS.get(value, "" if value == "NA" else value)


def number(value):
    return float("nan") if value in ("", "NA", "nan") else float(value)


def compare(reference, candidate, rtol, atol):
    problems = []
    if set(reference) != set(candidate):
        problems.append(f"specification IDs differ: {sorted(set(reference) ^ set(candidate))[:10]}")
    choice_columns = [c for c in next(iter(reference.values())) if c not in KEYS and c not in COUNTS
                      and c not in NUMBERS and c in next(iter(candidate.values()))]
    for spec_id in sorted(set(reference) & set(candidate)):
        a, b = reference[spec_id], candidate[spec_id]
        for column in KEYS:
            if column in a and column in b and norm(a[column]) != norm(b[column]):
                problems.append(f"{spec_id} {column}: {a[column]!r} vs {b[column]!r}")
        if a["status"] != "ok" or b["status"] != "ok":
            continue
        if a.get("sample_hash") != b.get("sample_hash"):
            problems.append(f"{spec_id} sample_hash differs")
        for column in COUNTS:
            if column not in a or column not in b:
                continue
            x, y = number(a[column]), number(b[column])
            if not (math.isnan(x) and math.isnan(y)) and x != y:
                problems.append(f"{spec_id} {column}: {x} vs {y}")
        se = abs(number(a["std_error"]))
        parameters = [c for c in a if c.startswith(("theta_", "se_")) and c in b]
        for column in NUMBERS + tuple(parameters):
            if column not in a or column not in b:
                continue
            x, y = number(a[column]), number(b[column])
            if math.isnan(x) and math.isnan(y):
                continue
            # Libraries evaluate t-distribution tails with different routines.
            tolerance = max(rtol, 1e-6) if column in ("p_value", "j_p_value") else rtol
            # Estimates and interval bounds near zero are judged on the scale of
            # their own standard error, not only relative to their (tiny) value.
            floor = se if column in ("estimate", "ci_lower", "ci_upper") and not math.isnan(se) else 1.0 if column == "statistic" else 0.0
            if column.startswith("theta_") and "se_" + column[6:] in a:
                floor = abs(number(a["se_" + column[6:]]))
            scale = max(abs(x), abs(y), floor)
            if not (abs(x - y) <= tolerance * scale or abs(x - y) <= atol):
                problems.append(f"{spec_id} {column}: {x!r} vs {y!r}")
    return problems, choice_columns


def summary(reference, candidate, close_tol):
    """Agreement shares for nonlinear searches (see the module docstring)."""
    ids = sorted(set(reference) & set(candidate))
    same_status = sum(reference[i]["status"] == candidate[i]["status"] for i in ids)
    both = [i for i in ids if reference[i]["status"] == "ok" and candidate[i]["status"] == "ok"]
    gaps = sorted(abs(number(reference[i]["estimate"]) - number(candidate[i]["estimate"])) /
                  max(abs(number(reference[i]["std_error"])), 1e-300) for i in both)
    close = sum(g <= close_tol for g in gaps)
    return {"rows": len(ids), "same_status": same_status, "both_ok": len(both), "close": close,
            "tight": sum(g <= 1e-8 for g in gaps), "max_gap": gaps[-1] if gaps else float("nan")}


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("reference")
    parser.add_argument("candidate")
    parser.add_argument("--rtol", type=float, default=1e-8)
    parser.add_argument("--atol", type=float, default=1e-12)
    parser.add_argument("--summary", action="store_true", help="report agreement shares (nonlinear searches)")
    parser.add_argument("--close-tol", type=float, default=1e-4)
    parser.add_argument("--min-status", type=float, default=0.0)
    parser.add_argument("--min-close", type=float, default=0.0)
    args = parser.parse_args()
    reference, candidate = load(args.reference), load(args.candidate)
    if args.summary:
        if set(reference) != set(candidate):
            print("Specification IDs differ.", file=sys.stderr)
            return 1
        s = summary(reference, candidate, args.close_tol)
        status_share = s["same_status"] / s["rows"]
        close_share = s["close"] / s["both_ok"] if s["both_ok"] else 0.0
        print(f"Same status: {s['same_status']} of {s['rows']} ({status_share:.1%}). Succeeded in both: {s['both_ok']}; "
              f"estimates within {args.close_tol:g} standard errors: {s['close']} ({close_share:.1%}); within 1e-8: "
              f"{s['tight']}; largest gap: {s['max_gap']:.3g} standard errors.")
        if status_share < args.min_status or close_share < args.min_close:
            print("Agreement below the stated minimum.", file=sys.stderr)
            return 1
        return 0
    problems, _ = compare(reference, candidate, args.rtol, args.atol)
    if problems:
        print(f"{len(problems)} discrepancies:\n" + "\n".join(problems[:50]), file=sys.stderr)
        return 1
    print(f"Agreement: {len(reference)} specifications match (rtol={args.rtol}, atol={args.atol}).")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
