# Prompt 1: Draft the menu

```text
Read docs/ai-guide.md sections 0-3 and paper/config/menu.schema.json.
Then read [PAPER: title, table, column] and its replication package at [PATH/URL].

Draft menu.json for a specification curve of [RESULT] without estimating anything.
1. State the estimand, units, and what must stay fixed (identifying comparison,
   required controls, treatment definition). Quote the article for each.
2. List candidate decisions that careful analysts in this literature vary. For each
   option give a one-sentence justification and a citation or reason. Mark options
   that change the estimand; they go in a panel_by dimension.
3. List combinations that make no sense and add them as exclusions with reasons.
4. Mark the article's own robustness specifications in original_specifications.
5. Report the grid size (rows per panel). If it exceeds a few hundred, propose
   which decisions to merge or drop, with reasons that do not depend on results.
6. Run the menu validator and fix every error.

Do not run any specification other than the published baseline. Do not choose
options based on how you expect them to change the estimate.
Return: menu.json, a table of decisions/options/justifications, and open questions.
```
