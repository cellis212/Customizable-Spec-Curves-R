# Prompt 5: Audit the outputs

```text
Audit [RUN_DIR] without changing anything.
- Confirm one row per spec_id, the baseline row matches the reference, and every
  failed row has a reason.
- Tabulate n_obs, n_clusters, and n_periods by dimension option; flag any option whose
  sample is much smaller than expected and explain why.
- Check that common-sample rows share a sample_hash within each filter set.
- Open the figure and check labels, units, the baseline marker, and the original-paper
  markers.
- For gmm rows, tabulate failure reasons and convergence codes by option, estimates at a
  parameter bound, J-test rejections, and start_share_best for multistart rows; say which
  options fail most and whether failed rows cluster in one part of the menu.
Return a list of problems (or "none found") with evidence for each.
```
