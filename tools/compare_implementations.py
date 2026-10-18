"""Compare two results.csv files from different implementations, row by row.

Usage: python3 tools/compare_implementations.py REFERENCE.csv CANDIDATE.csv [--rtol 1e-8]

Checks identical specification IDs, sample modes, preferred/original flags, panel
labels, model formulas, outcomes, uncertainty labels, filters, status, samples
(observation hashes and counts), and numerically close estimates, standard errors, and intervals (p-values to at
least 1e-6 relative, because libraries compute t tails differently).
Uses only the standard library so it runs wherever the implementations do.
"""

import argparse
import csv
import hashlib
import math
import os
import sys

KEYS = ("spec_id", "sample_mode", "preferred", "original", "panel", "formula", "outcome", "vcov", "filters", "status")
COUNTS = ("n_eligible", "n_obs", "n_units", "n_periods", "n_clusters")
NUMBERS = ("estimate", "std_error", "statistic", "p_value", "ci_lower", "ci_upper")
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
    return BOOLS.get(value, value)


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
        for column in NUMBERS:
            x, y = number(a[column]), number(b[column])
            # Libraries evaluate t-distribution tails with different routines.
            tolerance = max(rtol, 1e-6) if column == "p_value" else rtol
            if not math.isclose(x, y, rel_tol=tolerance, abs_tol=atol):
                problems.append(f"{spec_id} {column}: {x!r} vs {y!r}")
    return problems, choice_columns


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("reference")
    parser.add_argument("candidate")
    parser.add_argument("--rtol", type=float, default=1e-8)
    parser.add_argument("--atol", type=float, default=1e-12)
    args = parser.parse_args()
    reference, candidate = load(args.reference), load(args.candidate)
    problems, _ = compare(reference, candidate, args.rtol, args.atol)
    if problems:
        print(f"{len(problems)} discrepancies:\n" + "\n".join(problems[:50]), file=sys.stderr)
        return 1
    print(f"Agreement: {len(reference)} specifications match (rtol={args.rtol}, atol={args.atol}).")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
