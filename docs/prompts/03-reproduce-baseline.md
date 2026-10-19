# Prompt 3: Reproduce the published baseline

```text
Using the analysis CSV, estimate only the published baseline [TABLE, COLUMN] with a
direct call (fixest::feols / pyfixest.feols / reghdfe) using the article's controls,
fixed effects, sample, and clustering.
Report a table: published vs reproduced coefficient, standard error, N, clusters.
Propose a numerical tolerance and explain it (e.g., rounding in the published table).
If the estimate does not reproduce within tolerance, stop and list likely causes
(sample, definitions, data vintage). Do not change the menu to force agreement.
If it reproduces, fill menu.json "reference" with estimate, std_error, tolerance, source.
For a structural (gmm) application, first evaluate the moment functions at the published
parameter values and reproduce the published J statistic and standard errors (no
optimizer), then reproduce the estimate with the preferred row. If the original code's
optimizer failed or its output cannot be rebuilt, stop and agree the target with the
researcher before estimating anything.
```
