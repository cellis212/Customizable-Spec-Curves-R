# Prompt 4: Freeze, run, and validate

```text
The researcher has approved menu.json version [VERSION].
1. Freeze it: Rscript paper/run.R freeze menu.json input.csv manifest.json
2. Run it:   Rscript paper/run.R estimate manifest.json input.csv [RUN_DIR]
3. If a second runtime is available, run python -m speccurve run menu.json input.csv
   [RUN_DIR]-py and python3 tools/compare_implementations.py on the two results.csv.
Report: validation.json in full, the number of failed rows with reasons, the
comparison result, and the path to the figure. Do not edit the menu, data, or code
after freezing; if something must change, stop and propose a new version.
```
