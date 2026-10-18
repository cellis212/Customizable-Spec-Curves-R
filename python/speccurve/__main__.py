"""Command line: python -m speccurve run MENU.json INPUT.csv OUTPUT_DIR"""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path

import pandas as pd

from .engine import MenuError, file_sha256, freeze_manifest, read_menu, run_manifest, summarise_results, validate_results


def run_record(input_csv: str, input_hash: str) -> dict:
    import importlib.metadata as metadata
    import platform

    versions = {}
    for package in ("speccurve", "pyfixest", "pandas", "numpy", "scipy", "matplotlib"):
        try:
            versions[package] = metadata.version(package)
        except metadata.PackageNotFoundError:
            versions[package] = "not installed"
    return {"input_csv_sha256": input_hash, "python": platform.python_version(), "package_versions": versions,
            "freezing": "not implemented in the Python port; use the R engine to freeze manifests",
            "joint_inference": "not_implemented"}


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(prog="speccurve")
    sub = parser.add_subparsers(dest="command", required=True)
    run = sub.add_parser("run", help="Build the grid, estimate every specification, validate, and plot.")
    run.add_argument("menu")
    run.add_argument("input_csv")
    run.add_argument("output_dir")
    run.add_argument("--no-plot", action="store_true")
    args = parser.parse_args(argv)

    output = Path(args.output_dir)
    if (output / "results.csv").exists():
        print("Output already exists; use a fresh run directory.", file=sys.stderr)
        return 1
    output.mkdir(parents=True, exist_ok=True)
    menu = read_menu(args.menu)
    data = pd.read_csv(args.input_csv)
    manifest = freeze_manifest(menu, data)
    manifest["input_hash"] = file_sha256(args.input_csv)
    manifest["grid"].to_csv(output / "grid.csv", index=False)
    results = run_manifest(manifest, data)
    results.to_csv(output / "results.csv", index=False)
    try:
        validation = validate_results(manifest, results)
    except MenuError as error:
        validation = {"passed": False, "error": str(error), "joint_inference": "not_implemented"}
    (output / "validation.json").write_text(json.dumps(validation, indent=2, default=str))
    if not validation.get("passed"):
        print("Validation failed:", validation.get("error", validation.get("failed_ids")), file=sys.stderr)
        return 1
    summarise_results(manifest, results).to_csv(output / "descriptive_summary.csv", index=False)
    (output / "run_record.json").write_text(json.dumps(run_record(args.input_csv, manifest["input_hash"]), indent=2))
    if not args.no_plot:
        from .plot import plot_curve
        plot_curve(manifest, results, output)
    print(f"Validated {validation['completed_rows']} of {validation['expected_rows']} specifications.")
    print(f"Baseline reproduction: {validation['baseline_reproduction']}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
