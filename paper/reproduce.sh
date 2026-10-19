#!/bin/sh
# Reproduce the applications, figures, and tables from public data.
#
# Usage: sh paper/reproduce.sh RAW_DIR WORK_DIR [MANUSCRIPT_DIR]
#
# RAW_DIR must contain the unmodified downloads (hashes are checked):
#   bll_2010/bbb.zip                     DataverseNL doi:10.34894/B1K9OU, file bbb.zip
#   cz_2021/PredictorPortsFull.csv and the twelve PredictorAltPorts_*.zip files from the
#   April 2021 Open Source Asset Pricing release, with cz_2021/provenance.json listing
#   their SHA-256 hashes (see paper/applications/cz_am/README.md)
#   cz_2025/ the corresponding October 2025 release files with provenance.json
#   (see paper/applications/cz_bliq/README.md)
#   btmr_2021/ the Harvard Dataverse package doi:10.7910/DVN/LQPRIA (original formats)
#   fi_2015/20121416_data.zip            AEA package for Favara and Imbs (2015)
#   bp_2005/june05_data_burgess.zip      AEA package for Burgess and Pande (2005)
#   cfr_2010/recursivepanel.dta.zip      Cellini, Ferreira and Rothstein (2010) archive
#   (see each paper/applications/<id>/README.md for sources and hashes)
# WORK_DIR receives processed inputs, frozen manifests, runs, figures, tables, and
# explorer/specification_explorer.html, a searchable page with every specification.
# MANUSCRIPT_DIR (optional) is a LaTeX manuscript kept outside this repository;
# if given, figures and tables are copied into its figures/ and tables/ folders.
# Existing manifests and runs are reused, never overwritten: a frozen grid is
# estimated once. Delete a run directory yourself to re-estimate it.
set -eu
root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
raw=$1
work=$2
processed="$work/data/processed"
estimates="$work/estimates"
mkdir -p "$processed/bll_2010" "$processed/cz_am" "$processed/cz_bliq" "$processed/btmr2021" "$estimates/bll2010" \
  "$estimates/cz_am" "$estimates/cz_bliq" "$estimates/btmr2021" "$work/figures" "$work/tables"
for app in fi2015 cfr2010 bp2005; do mkdir -p "$processed/$app" "$estimates/$app"; done
cd "$root"

bll_input="$processed/bll_2010/bll_input.csv"
cz_input="$processed/cz_am/cz_am_input.csv"
bliq_input="$processed/cz_bliq/cz_bliq_input.csv"
[ -f "$bll_input" ] || Rscript paper/applications/bll2010/build_input.R "$raw/bll_2010/bbb.zip" "$bll_input"
[ -f "$cz_input" ] || Rscript paper/applications/cz_am/build_input.R "$raw/cz_2021" "$cz_input"
[ -f "$bliq_input" ] || Rscript paper/applications/cz_bliq/build_input.R "$raw/cz_2025" "$bliq_input"
btmr_input="$processed/btmr2021/btmr_input.csv"
[ -f "$btmr_input" ] || Rscript paper/applications/btmr2021/build_input.R "$raw/btmr_2021" "$btmr_input"
fi_input="$processed/fi2015/fi2015_input.csv"
cfr_input="$processed/cfr2010/cfr2010_input.csv"
bp_input="$processed/bp2005/bp2005_input.csv"
[ -f "$fi_input" ] || Rscript paper/applications/fi2015/build_input.R "$raw/fi_2015" "$fi_input"
[ -f "$cfr_input" ] || Rscript paper/applications/cfr2010/build_input.R "$raw/cfr_2010" "$cfr_input"
[ -f "$bp_input" ] || Rscript paper/applications/bp2005/build_input.R "$raw/bp_2005" "$bp_input"

run_application() {
  app=$1; input=$2; version=$3; rtol=${4:-1e-8}
  manifest="$estimates/$app/manifest-$version.json"
  run="$estimates/$app/run-$version"
  if [ ! -f "$manifest" ]; then
    Rscript paper/run.R validate "paper/applications/$app/menu.json" "$input"
    Rscript paper/run.R freeze "paper/applications/$app/menu.json" "$input" "$manifest"
  fi
  [ -f "$run/results.csv" ] || Rscript paper/run.R estimate "$manifest" "$input" "$run"
  if [ -x python/.venv/bin/python ] && [ ! -f "$run-python/results.csv" ]; then
    python/.venv/bin/python -W ignore -m speccurve run "paper/applications/$app/menu.json" "$input" "$run-python" --no-plot
  fi
  if [ -f "$run-python/results.csv" ]; then
    python3 tools/compare_implementations.py "$run/results.csv" "$run-python/results.csv" --rtol "$rtol"
  fi
}
run_application bll2010 "$bll_input" bll-v1
run_application cz_am "$cz_input" cz-am-v1
run_application cz_bliq "$bliq_input" cz-bliq-v1
run_application btmr2021 "$btmr_input" btmr-v1
# Supplementary design examples. The RD regressors (raw cubic vote shares near
# 10^5 beside binary terms) limit R-Python agreement to about 1e-6.
run_application fi2015 "$fi_input" fi-v1
run_application cfr2010 "$cfr_input" cfr-v1 1e-6
run_application bp2005 "$bp_input" bp-v1 1e-7

Rscript paper/figures/make_figures.R "$estimates" "$work/figures"
Rscript paper/figures/supplement_tables.R "$estimates" "$work/tables"
Rscript paper/figures/paper_numbers.R "$estimates" "$work/tables/paper_numbers.json"
# One self-contained HTML page with every specification of every application, for an online appendix.
Rscript paper/figures/make_explorer.R "$work/explorer/specification_explorer.html" --applications "$estimates" \
  --title "Specification Curves in Empirical Finance: every specification"

if [ $# -ge 3 ]; then
  manuscript=$3
  mkdir -p "$manuscript/figures" "$manuscript/tables"
  cp "$work"/figures/Figure_*.pdf "$manuscript/figures/"
  cp "$work"/tables/table_*.tex "$manuscript/tables/"
  echo "Copied figures and tables into $manuscript"
fi
echo "Figures and tables: $work/figures, $work/tables; explorer: $work/explorer/specification_explorer.html"
