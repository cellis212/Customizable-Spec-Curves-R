# Design example: staggered adoption (Favara and Imbs, 2015)

Favara and Imbs (2015), "Credit Supply and the Price of Housing," American Economic Review 105(3), 958-992. County-year growth in mortgage originations on the lagged interstate branching deregulation index (0-4 restrictions lifted, adopted by states in different years between 1994 and 2005), with county and year fixed effects, clustered by state. Banks are covered by the branching rules; thrifts, credit unions, and independent mortgage companies are a placebo group.

The frozen menu `fi-v1` (432 specifications) was written before any alternative was estimated. Its reference is Table 2, Panel A, column 1: 0.028 (0.010), N = 10,992, reproduced as 0.02776 (0.01050) with 50 state clusters. The placebo column (0.002, 0.008) is marked as an original specification.

## Data (not redistributed)

AEA replication package `20121416_data.zip` (openICPSR record doi:10.3886/E112894V1), downloadable without a login from `https://assets.aeaweb.org/asset-server/articles-attachments/aer/data/10503/20121416_data.zip`. Save it unmodified in `RAW/fi_2015/`; the build script checks SHA-256 `93af8759fffddc9f7ee68e0b6e65d1cb7993b2405196ac4d7f74ab8c0ff3decd`.

## Design notes

- The input is long by lender group (`lender` 1 = banks, 2 = placebo lenders), so the placebo panel runs through the same menu.
- Adoption cohorts are each state's first year with any deregulation (contemporaneous index): 1995-1996 (13 states), 1997 (9), 1998 (16), 2000-2001 (4), never by 2005 (8). The cohort dimension leaves one cohort out at a time, a cheap check on which two-way fixed-effects comparisons drive the estimate.
- Heterogeneity-robust estimators (Callaway-Sant'Anna; de Chaisemartin-D'Haultfoeuille for a non-binary treatment) target different averages and are not in this curve.
