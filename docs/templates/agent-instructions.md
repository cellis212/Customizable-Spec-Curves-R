# Specification-curve project instructions for AI assistants

Copy this file into your own project as the instructions file your assistant reads
(for example `AGENTS.md` or `CLAUDE.md`) and fill in the brackets.

## Project
- Result under study: [PAPER, TABLE, COLUMN, COEFFICIENT]
- Estimand and units: [ ... ]
- Fixed design elements: [identifying comparison, required controls, treatment definition]
- Data: [location, vintage, licence]; raw files are read-only.
- Workflow: https://github.com/cellis212/Customizable-Spec-Curves-R (docs/ai-guide.md)

## Rules
1. Never invent data, estimates, citations, or command output.
2. Reproduce the published baseline before freezing; report the tolerance.
3. Do not add, drop, or change alternatives after seeing curve estimates. Changes
   require a new menu version and a reason recorded in the menu's notes.
4. Keep every specification, including failures, with reasons.
5. Keep different estimands in separate panels; keep fixed design elements fixed.
6. Describe the curve; do not treat significance shares as hypothesis tests.
7. Do not commit data, outputs, credentials, or transcripts to public repositories.
8. Ask the researcher before: changing the estimand, changing the baseline, or
   freezing a menu.
