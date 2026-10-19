# Building specification curves with an AI coding assistant

This guide is written for two readers at once: a researcher who wants a specification curve for a finance result, and the AI coding assistant (Claude Code, Codex, Cursor, Copilot, or similar) that will write most of the code. The division of labour is deliberate:

| The researcher decides | The AI assistant does |
|---|---|
| The economic question, units, and identifying comparison | Reads the paper and replication package and drafts the menu |
| Which published number is the baseline | Writes the data-construction script and reproduces the baseline |
| Which alternatives are defensible, and why | Encodes them in `menu.json`, validates it, and flags redundancies |
| When the menu is final | Freezes it, runs every specification, and checks every output |
| What the curve means | Drafts the figure, summary table, and a neutral results paragraph |

The engines in this repository are small on purpose. Everything study-specific lives in two files the assistant writes: a data script that produces one analysis CSV, and a declarative `menu.json`. The engines then enforce the rules that make AI-written analysis trustworthy: the baseline must reproduce, the grid is frozen with hashes before estimation, every row is kept, and changes require a new version.

## 0. Ground rules for the assistant

Paste these into your assistant's project instructions (see `docs/templates/agent-instructions.md`).

1. Never invent data, estimates, citations, or command output. If a command was not run, say so.
2. Do not choose, add, or drop alternatives after seeing curve estimates. Changes after freezing need a new menu version and a stated reason.
3. Reproduce the published baseline before freezing the grid. Report the published number, your number, the difference, and the tolerance.
4. Keep failed specifications. Explain each failure; never delete a row to make validation pass.
5. Keep different estimands apart (`panel_by`), and keep the identifying comparison and required controls fixed.
6. Treat shares of significant estimates as descriptions of the menu, not as hypothesis tests.
7. Do not commit data, generated outputs, credentials, or conversation logs to a public repository.

## 1. Inputs to collect from the researcher

Before writing code, the assistant should ask for (or extract and confirm):

- **Question and estimand.** For example: "the effect of intrastate branch deregulation on the state Gini coefficient, in log points".
- **Baseline.** The exact table, column, coefficient, standard error, sample, and clustering in the published paper.
- **Data.** Where the replication package or raw data live, their vintage, and any licence restrictions.
- **Variables.** Definitions of the outcome, focal variable, required controls, and fixed effects.
- **Design.** What must not change: the identifying comparison, required controls, and treatment definition.
- **Candidate alternatives.** Choices other careful analysts make in this literature: sample windows, filters, controls, fixed-effect structures, trends, outcome transformations, weighting, breakpoints, holding periods.
- **Uncertainty.** The dependence structure: clustered panel, serially correlated time series, or few clusters.

## 2. Choose the language and check support

| Language | Engine | Run command |
|---|---|---|
| R (reference) | `paper/R/curve.R` (fixest) | `Rscript paper/run.R freeze MENU INPUT MANIFEST`, then `Rscript paper/run.R estimate MANIFEST INPUT OUTDIR` |
| Python | `python/speccurve` (pyfixest) | `python -m speccurve run MENU INPUT OUTDIR` |
| Stata | `stata/speccurve_run.do` (reghdfe) | `Rscript paper/run.R plan MANIFEST INPUT PLAN.csv`, then in Stata `do stata/speccurve_run.do PLAN.csv INPUT OUTDIR` |

R and Python read the same `menu.json`; Stata executes the plan that R exports from a frozen manifest. All three write `results.csv` with the same specification IDs, so results can be compared with `tools/compare_implementations.py`. The R and Python engines have been checked against each other on all seven applications in the paper and supplement (2,594 specifications). The Stata executor has not yet been run against them, and it refuses weights and instrumental variables; check it on the demo before relying on it.

**Supported designs.** Linear models with any number of fixed effects (`linear_fixed_effects`, with clustered, heteroskedasticity-robust, or i.i.d. errors), two-stage least squares with fixed effects (`iv_fixed_effects`: `treatment` is the endogenous regressor and `instruments` lists the excluded instruments; every row reports the Montiel Olea-Pflueger effective first-stage F), and means of a single time series such as a long-short portfolio return (`time_series_mean`, with i.i.d. or Newey–West errors). Regressions accept positive `weights` (for example kernel weights in a regression discontinuity). Unit-specific linear trends are available in R and Python (Python matches fixest's degrees-of-freedom count).

**Design-specific choices.** Put the choices the design itself creates into the menu: in staggered adoption, leave out one adoption cohort at a time and drop never-treated units (filters on a cohort variable built in the data script); in a regression discontinuity, vary the bandwidth (filters on the absolute running variable), the kernel (weights built in the data script for each bandwidth), the polynomial, and a donut; in instrumental variables, leave one instrument out (move it to the controls) and leave one cluster out, and read the effective F for every row. A placebo group or outcome belongs in the curve as its own panel. See `paper/applications/fi2015`, `cfr2010`, and `bp2005`.

**Not supported.** Joint inference across specifications, multiway clustering, few-cluster corrections, weak-instrument-robust (Anderson-Rubin) intervals, data-driven RD bandwidths with bias-corrected intervals, nonlinear and structural (GMM) estimators, and heterogeneity-robust staggered-adoption estimators. If the design needs one of these, the assistant should say so and treat it as a separate, separately validated analysis. It should not substitute a supported estimator silently.

## 3. Design the menu

A menu (`menu.json`, schema in `paper/config/menu.schema.json`) has a **base** model and a list of **dimensions**. Each dimension is one decision; each **option** is one defensible answer to it. The grid is the Cartesian product of all options, minus documented **exclusions**.

A shortened version of the menu used in the paper (`paper/applications/bll2010/menu.json` has all five decisions and three outcome panels):

```json
{
  "schema_version": 2,
  "version": "bll-v1",
  "analysis_kind": "application",
  "estimator": "linear_fixed_effects",
  "title": "Branch deregulation and income inequality",
  "estimand": "Change in the state Gini coefficient after intrastate branching deregulation",
  "estimate_label": "Effect on Gini",
  "row_id": "row_id", "unit": "state", "time": "year",
  "base": {
    "outcome": "log_gini", "treatment": "deregulated",
    "required_controls": [], "fixed_effects": ["state", "year"],
    "vcov": {"type": "cluster", "cluster": "state"}, "filters": []
  },
  "confidence_level": 0.95,
  "dimensions": [
    {"name": "window", "label": "Sample window", "options": [
      {"id": "y2006", "label": "1976-2006", "justification": "Published sample."},
      {"id": "y1999", "label": "1976-1999", "justification": "BLL robustness check; 1999 is the last reform year.",
       "filters": [{"variable": "year", "op": "<=", "value": 1999}]}
    ]}
  ],
  "sample_modes": ["available"],
  "preferred": {"window": "y2006", "sample_mode": "available"},
  "reference": {"estimate": -0.022, "tolerance": 0.0005,
                "source": "Beck, Levine and Levkov (2010), Table II, Panel A, column 2 (log Gini)"}
}
```

Option fields:

| Field | Effect | Combines across dimensions? |
|---|---|---|
| `add_controls` | Adds regressors | Yes, they accumulate |
| `filters` | Restricts the sample (`==`, `!=`, `>=`, `<=`, `>`, `<`, `in`, `not_in`) | Yes, all filters apply |
| `outcome`, `treatment` | Replaces the variable | Only one dimension may set each |
| `fixed_effects` | Replaces the fixed-effect list | Only one dimension |
| `unit_trend` | Adds unit-specific linear trends | Only one dimension |
| `vcov` | Replaces the uncertainty convention | Only one dimension |

Design rules the engines enforce: variables must exist and be numeric where required; option IDs must be unique; the preferred and `original_specifications` rows must be in the grid; two rows may not define the same model and sample; every exclusion needs a reason.

Design rules the assistant must enforce itself, because no code can:

- Write a one-sentence `justification` for every option. If you cannot, the option does not belong.
- An option that changes the estimand (a different outcome, a log instead of a level) goes into a dimension named by `panel_by`, which gives it its own panel and scale.
- List the specifications the original article reported in `original_specifications`, so the figure can mark them.
- Use `sample_modes: ["available", "common"]` when options add variables with missing values.

Validate before showing the researcher:

```sh
Rscript paper/run.R validate MENU INPUT
```

This builds the grid and reports the number of rows, exclusions, and any errors, without estimating anything.

## 4. Build the input and reproduce the baseline

The assistant writes one script that turns the raw package into a single analysis CSV with a unique `row_id`. It should:

- Preserve raw downloads unchanged and record their source URL, date, and SHA-256 hash.
- Report observation counts at every merge and restriction.
- Use simple variable names (letters, digits, underscores).

Then it reproduces the published baseline with a direct, minimal regression and compares coefficient, standard error, observation count, and number of clusters with the article. Record the published estimate and a numerical `tolerance` in `reference`. If the baseline does not reproduce, stop and report; do not tune the data until it does.

## 5. Freeze and run

```sh
Rscript paper/run.R freeze menu.json input.csv manifest.json
Rscript paper/run.R estimate manifest.json input.csv run-001
```

Freezing writes the full grid, the input hash, and the analysis-source hashes. Estimation refuses to run if any of them changed. A new idea after freezing means a new `version` and a new manifest; keep the old one.

## 6. Check every output

The run directory contains:

| File | Check |
|---|---|
| `validation.json` | `passed` is true, `baseline_reproduction` is `passed`, and `completed_rows` equals `expected_rows` |
| `results.csv` | One row per `spec_id`; `n_obs`, `n_clusters`, and `sample_hash` are plausible; common-sample rows share a hash |
| `specification_curve.pdf` | The baseline marker sits at the published estimate; the panels are labelled in economic units |
| `descriptive_summary.csv` | Median, range, share positive, and shares significant per panel |

Cross-check against a second implementation when possible:

```sh
python -m speccurve run menu.json input.csv run-001-py
python3 tools/compare_implementations.py run-001/results.csv run-001-py/results.csv
```

## 7. Interpret

The assistant drafts and the researcher edits. A good paragraph answers four questions in this order:

1. Where does the published estimate sit in the distribution (its percentile, and the median and range)?
2. Does the sign hold, and how many pointwise intervals exclude zero?
3. Which decisions move the estimate? Read the dot pattern in the lower panel.
4. Is the dispersion large relative to the economic magnitude of the effect?

Avoid: "X% of specifications are significant, so the result is robust" (a menu description, not a test); "the curve proves" (it describes); and any claim about specifications that were not run.

## 8. Report template

> We estimate N specifications that cross [dimensions]. The published estimate of [b] lies at the [p]th percentile of the curve (median [m], range [lo] to [hi], in [units]). All/most/some estimates are [sign]; [k] of N pointwise 95% intervals exclude zero. The main source of dispersion is [decision]: specifications that [choice] produce estimates [higher/lower] by about [amount]. [One sentence on economic magnitude.] Intervals are pointwise and the summaries describe the stated menu; we do not report joint inference.

**Online appendix.** A figure shows the curve; readers and referees also need every row. After validation, write a searchable explorer of all specifications:

```sh
Rscript paper/figures/make_explorer.R appendix.html manifest.json run-001 [manifest-2.json run-002 ...] --title "Online appendix"
```

The output is one self-contained HTML file: data, styles, and code are inline, so it needs no server, internet connection, or software beyond a web browser, and it can be posted beside the article, deposited with the replication package, or hosted on a personal site. Readers can search every specification by any text (an option, a variable, a sample filter, a specification ID), filter by each decision, see the selection highlighted on the full curve or re-ranked on its own, compare median estimates across options, open any specification for its choices and their justifications, formula, sample sizes, observation hash, and failure message, and download the selected rows as CSV. The page address records the current view, so a link can send readers to one subset (for example, only the region-by-year specifications). For a Python run, pass its `menu.json` instead of a manifest; the run's `grid.csv` is then used to check completeness. The script refuses results that lack a frozen row or that came from a different manifest. Regenerate the file after any change instead of editing it by hand, and tell the researcher where it was written; posting it is the researcher's decision.

## 9. Troubleshooting

| Message | Meaning and fix |
|---|---|
| `Menu references variables absent from input data` | Rename in the data script or the menu; names must match exactly. |
| `Only one dimension may replace outcome` | Put all outcome alternatives in one dimension. |
| `Two grid rows define the same model and sample` | Two options are identical in effect. Remove one or add an exclusion. |
| `Preferred specification does not reproduce the documented published baseline` | Stop. Fix the data or baseline definition, then refreeze under a new version. |
| `Analysis source differs from the frozen manifest` | Code changed after freezing. Refreeze under a new version. |
| `Focal coefficient is unidentified or absorbed` | The treatment is collinear with the fixed effects in that row. The row stays in the results as failed. |

## 10. Prompt templates

Ready-to-paste prompts for each step are in `docs/prompts/`. Each one tells the assistant what to read, what to produce, and what it may not do.
