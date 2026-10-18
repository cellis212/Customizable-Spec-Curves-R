# Application 1: Beck, Levine and Levkov (2010)

Branch deregulation and income inequality; state-year panel, two-way fixed effects, standard errors clustered by state.

## Data (not redistributed)

Download `bbb.zip` (769,264 bytes) from the authors' DataverseNL archive, doi:[10.34894/B1K9OU](https://doi.org/10.34894/B1K9OU) (version 7.0, CC BY 4.0):

```sh
mkdir -p RAW/bll_2010
curl -L -o RAW/bll_2010/bbb.zip "https://dataverse.nl/api/access/datafile/1782"
```

The build script checks SHA-256 `f49d78db6be4e284c5b53d52949f07f072884d080df79780bd61d585d0093cd9`.

## Files

- `build_input.R` builds the analysis CSV from `bbb/macro_workfile.dta`, following the authors' `TableII.do` and adding the menu's alternative variables (treatment from the reform year, Census region-by-year identifiers, pre-1976 reform indicator).
- `menu.json` is the frozen menu `bll-v1`: three outcome panels (log Gini, logistic Gini, log Theil) crossed with controls, window, states, time effects, and treatment timing; 324 specifications. The baseline is BLL Table II, Panel A, column 2 (log Gini, $-0.022$, s.e. 0.008).

## Run

```sh
Rscript paper/applications/bll2010/build_input.R RAW/bll_2010/bbb.zip WORK/bll_input.csv
Rscript paper/run.R validate paper/applications/bll2010/menu.json WORK/bll_input.csv
Rscript paper/run.R freeze   paper/applications/bll2010/menu.json WORK/bll_input.csv WORK/manifest-bll-v1.json
Rscript paper/run.R estimate WORK/manifest-bll-v1.json WORK/bll_input.csv WORK/run-bll-v1
```

Or run everything, including the second application and the manuscript, with `sh paper/reproduce.sh RAW WORK`.

After freezing, only the menu's `notes` field was edited (to point to the published Supplementary Material instead of an internal record); the grid, options, and reference are unchanged.
