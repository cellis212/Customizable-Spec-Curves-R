# Prompt 2: Build the analysis input

```text
Read docs/ai-guide.md section 4.
Write [scripts/build_input.R or .py] that turns the raw files at [PATH] into one
analysis CSV with a unique row_id and every variable named in menu.json.
- Never modify raw files. Record each raw file's source URL, access date, and SHA-256.
- Print observation counts before and after every merge, filter, and transformation,
  and the number of missing values for each menu variable.
- Use the article's variable definitions; quote them in comments.
- Write the CSV and a provenance JSON next to it (outside the public repository).
Run the script and paste its full printed output. Do not estimate any model yet.
```
