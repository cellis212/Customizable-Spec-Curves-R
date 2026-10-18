"""Ordered estimates above an aligned choice panel (matplotlib)."""

from __future__ import annotations

from pathlib import Path
from typing import Any

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt  # noqa: E402
import pandas as pd  # noqa: E402

COLOURS = {"Published baseline": "#eb6834", "Reported in original": "#2a78d6", "Additional combination": "#8c8b86"}
MARKERS = {"Published baseline": "D", "Reported in original": "^", "Additional combination": "o"}


def plot_curve(manifest: dict[str, Any], results: pd.DataFrame, directory: str | Path,
               file_stem: str = "specification_curve") -> None:
    menu = manifest["menu"]
    results = results[results["status"] == "ok"].copy()
    results["role"] = ["Published baseline" if p else "Reported in original" if o else "Additional combination"
                       for p, o in zip(results["preferred"], results["original"])]
    multi = len(menu["sample_modes"]) > 1
    results["facet"] = results["panel"] + (" (" + results["sample_mode"] + " sample)" if multi else "")
    facets = list(dict.fromkeys(results.sort_values(["sample_mode"], kind="mergesort")["facet"]))
    dims = [d for d in menu["dimensions"] if d["name"] != menu.get("panel_by")]
    options = [(d.get("label", d["name"]), d["name"], o["id"], o["label"]) for d in dims for o in d["options"]]
    if multi:
        options += [("Sample", "sample_mode", m, f"{m} observations") for m in menu["sample_modes"]]
    size = menu.get("figure") or {}
    fig, axes = plt.subplots(2, len(facets), figsize=(size.get("width", 7), size.get("height", 6)), squeeze=False,
                             gridspec_kw={"height_ratios": [1.15, max(0.6, len(options) * 0.085)]})
    for j, facet in enumerate(facets):
        rows = results[results["facet"] == facet].sort_values(["estimate", "spec_id"], kind="mergesort")
        rank = range(1, len(rows) + 1)
        top, bottom = axes[0, j], axes[1, j]
        top.axhline(0, color="0.55", linewidth=0.6)
        top.vlines(list(rank), rows["ci_lower"], rows["ci_upper"], color="0.72", linewidth=0.6)
        for role, colour in COLOURS.items():
            mask = (rows["role"] == role).to_numpy()
            if mask.any():
                top.scatter([r for r, m in zip(rank, mask) if m], rows["estimate"][mask], color=colour,
                            marker=MARKERS[role], s=14 if role == "Additional combination" else 30, label=role, zorder=3)
        top.set_title(facet, fontsize=8, loc="left", fontweight="bold")
        top.set_xticks([])
        if j == 0:
            top.set_ylabel(menu.get("estimate_label", "Estimate"), fontsize=8)
        for k, (_, name, option_id, _) in enumerate(options):
            chosen = (rows[name] == option_id).to_numpy()
            bottom.scatter([r for r, c in zip(rank, chosen) if not c], [k] * int((~chosen).sum()), color="0.9", s=4)
            bottom.scatter([r for r, c in zip(rank, chosen) if c], [k] * int(chosen.sum()), color="#1f2a33", s=6)
        bottom.set_yticks(range(len(options)))
        bottom.set_yticklabels([o[3] for o in options] if j == 0 else [], fontsize=6)
        bottom.invert_yaxis()
        bottom.set_xticks([])
        for ax in (top, bottom):
            for spine in ("top", "right", "bottom"):
                ax.spines[spine].set_visible(False)
            ax.tick_params(labelsize=7)
    axes[0, 0].legend(frameon=False, fontsize=7, loc="upper left")
    fig.supxlabel("Specifications ranked by estimate", fontsize=8)
    fig.tight_layout()
    directory = Path(directory)
    fig.savefig(directory / f"{file_stem}.png", dpi=300)
    fig.savefig(directory / f"{file_stem}.pdf")
    plt.close(fig)
