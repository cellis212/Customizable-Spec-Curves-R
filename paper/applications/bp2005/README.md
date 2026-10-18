# Design example: instrumental variables (Burgess and Pande, 2005)

Burgess and Pande (2005), "Do Rural Banks Matter? Evidence from the Indian Social Banking Experiment," American Economic Review 95(3), 780-795. Rural bank branches per capita in 16 Indian states (1961-2000) are instrumented by 1961 branches per capita interacted with trend breaks in 1977 and 1990, when a licensing rule required four branches in unbanked locations for each new branch in a banked one. The outcome is the rural poverty headcount; state and year fixed effects; clustered by state.

The frozen menu `bp-v1` (1,224 specifications, estimator `iv_fixed_effects`) was written before any alternative was estimated. Its reference is Table 3, column 3: -4.74 (1.79), N = 627, reproduced as -4.743 (1.766) with 16 clusters (Stata's `reg` counts the state indicators in its small-sample correction).

## Data (not redistributed)

AEA replication package `june05_data_burgess.zip` (openICPSR record doi:10.3886/E112313V1), downloadable without a login from `https://assets.aeaweb.org/asset-server/articles-attachments/aer/data/june05_data_burgess.zip`. Save it unmodified in `RAW/bp_2005/`; the build script checks SHA-256 `caed05e13c700322192d827afe9fdd175e4a7ea6fa8acdbbce4560f22f3b059f`.

## Design notes

- Every row reports the Montiel Olea-Pflueger effective first-stage F under the same clustered covariance (`first_stage_f`); the supplement figure fades rows below 10.
- "Post-1976 trend only" and "post-1989 trend only" leave one instrument out; the other trend enters as a control, relaxing its exclusion restriction.
- 16 clusters (15 with a state left out); intervals use t critical values with G-1 degrees of freedom. Few-cluster bootstrap inference and weak-instrument-robust (Anderson-Rubin) intervals are not implemented.
