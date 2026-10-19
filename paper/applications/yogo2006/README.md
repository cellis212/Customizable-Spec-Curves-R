# Design example: structural estimation by GMM (Yogo, 2006)

Yogo (2006), "A Consumption-Based Explanation of Expected Stock Returns," Journal of Finance 61(2), 539-580. A representative household has Epstein-Zin utility over a CES aggregate of nondurable consumption and the stock of durables. Five preference parameters (EIS sigma, risk aversion gamma, intratemporal elasticity rho, durable weight alpha, discount factor beta) are estimated by two-step GMM from the Euler equations of the 3-month T-bill and of the test assets' excess returns and from the intratemporal first-order condition, quarterly 1951:1-2001:4. The focal parameter is gamma.

The frozen menu `yogo-v1` (1,440 specifications, estimator `gmm`) was written before any alternative was estimated. Borri and Ragusa (2017, Critical Finance Review 6(2), 381-393) show that the first step of Yogo's GAUSS code stops at an infeasible parameter value without reporting an error, so Table II rests on that path. The reproduction gate therefore has two parts:

1. `check_published.R`: at the second-step vectors Yogo's code saved (`results/*/nlest_dur.fmt` in the download), the moments and VARHAC reproduce the J statistics and standard errors of all five Table II columns to the printed three decimals (and the saved full-precision values to about 1e-8).
2. The preferred row builds the second-step weighting matrix at Yogo's saved first-step vector and minimizes from there: gamma = 191.438 (SE 49.868), as published.

## Data (not redistributed)

From Yogo's research page (`https://sites.google.com/site/motohiroyogo/research/asset-pricing`, "A Consumption-Based Explanation of Expected Stock Returns", links "Data" and "Replication files"): `CBased_Programs.zip` (501,339 bytes) and `CBased_Data.xls`. Save them unmodified in `RAW/yogo_2006/`; the scripts check SHA-256 `a707edc0ac3e8dcba2412259fa5179d5534ed8fc2d67295ccab6154808862020` for the zip. The input is built from the GAUSS matrices the estimation program loads (`data/Gauss/*.fmt` inside the zip); the workbook repeats the consumption series only.

## Files

- `build_input.R`: one row per quarter: consumption-side series under both timing conventions (`*_b`, Yogo's beginning of period; `*_e`, lagged one quarter), the T-bill, the market, the conditioning variables, and the 74 test portfolios.
- `moments.R`, `moments.py`: the moment functions (twins; pinned in the manifest).
- `check_published.R`: gate part 1.
- `menu.json`: the frozen menu.

## Design notes

- Search: "as published" uses the saved GAUSS first-step vector and searches the later steps locally; "local" runs one local search per step from Yogo's starting values; "multistart" adds 20 Halton points over the bounds in every step. The local algorithm is the same projected Levenberg-Marquardt routine in R and Python.
- Rows whose search does not converge are kept with their reason (`allow_failed_rows`); every row reports `convergence`, `bound_hits`, the J statistic and p-value (two-step, iterated and CUE), all parameters and standard errors, and `start_share_best` and `start_focal_range` across starts.
- CUE with VARHAC and identity weighting with the published first step are excluded (discontinuous objective; no estimator defined). Dropping both the T-bill equation and the FOC is not an option: beta would not be identified.
- Long-run covariances use uncentered moments, as Yogo's code does. Intervals are pointwise and normal; identification-robust GMM inference is not implemented.
