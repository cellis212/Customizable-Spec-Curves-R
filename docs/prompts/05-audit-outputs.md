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
Return a list of problems (or "none found") with evidence for each.
```
