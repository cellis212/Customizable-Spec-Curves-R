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
# WORK_DIR receives processed inputs, frozen manifests, runs, figures, and tables.
# MANUSCRIPT_DIR (optional) is a Quarto manuscript kept outside this repository;
# if given, figures and tables are copied into it and it is rendered.
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
cd "$root"

bll_input="$processed/bll_2010/bll_input.csv"
cz_input="$processed/cz_am/cz_am_input.csv"
bliq_input="$processed/cz_bliq/cz_bliq_input.csv"
[ -f "$bll_input" ] || Rscript paper/applications/bll2010/build_input.R "$raw/bll_2010/bbb.zip" "$bll_input"
[ -f "$cz_input" ] || Rscript paper/applications/cz_am/build_input.R "$raw/cz_2021" "$cz_input"
[ -f "$bliq_input" ] || Rscript paper/applications/cz_bliq/build_input.R "$raw/cz_2025" "$bliq_input"
btmr_input="$processed/btmr2021/btmr_input.csv"
[ -f "$btmr_input" ] || Rscript paper/applications/btmr2021/build_input.R "$raw/btmr_2021" "$btmr_input"

run_application() {
  app=$1; input=$2; version=$3
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
    python3 tools/compare_implementations.py "$run/results.csv" "$run-python/results.csv"
  fi
}
run_application bll2010 "$bll_input" bll-v1
run_application cz_am "$cz_input" cz-am-v1
run_application cz_bliq "$bliq_input" cz-bliq-v1
run_application btmr2021 "$btmr_input" btmr-v1

Rscript paper/figures/make_figures.R "$estimates" "$work/figures"
Rscript paper/figures/supplement_tables.R "$estimates" "$work/tables"
Rscript paper/figures/paper_numbers.R "$estimates" "$work/tables/paper_numbers.json"

if [ $# -ge 3 ]; then
  manuscript=$3
  mkdir -p "$manuscript/figures" "$manuscript/generated"
  cp "$work"/figures/Figure_*.pdf "$manuscript/figures/"
  cp "$work"/tables/table_*.tex "$manuscript/generated/"
  [ -f "$manuscript/word_count.py" ] && python3 "$manuscript/word_count.py" "$manuscript"
  (cd "$manuscript" && quarto render)
  echo "Manuscript outputs: $manuscript/output/"
fi
echo "Figures and tables: $work/figures, $work/tables"
