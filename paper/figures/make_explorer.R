#!/usr/bin/env Rscript
# Searchable specification-curve explorer for an online appendix. Writes ONE
# self-contained HTML file from frozen manifests (or menus) and their results.
# Data, styles, and code are inline, so the page needs no server, network
# connection, or software beyond a web browser, and a journal, data archive, or
# personal website can host it as it is. Readers can search every specification,
# filter by each decision, see the selection against the whole curve, open any
# specification's details, download the selected rows, and share a link to the
# current view. This script estimates nothing, and it refuses to write a page
# unless every frozen specification is present.
#
# Usage:
#   Rscript paper/figures/make_explorer.R OUTPUT.html MANIFEST.json RUN_DIR [MANIFEST.json RUN_DIR ...]
#   Rscript paper/figures/make_explorer.R OUTPUT.html --applications ESTIMATES_DIR
#   Option: --title "Page title"
# MANIFEST.json is a frozen R manifest, or the menu.json of a Python run (whose
# RUN_DIR also holds grid.csv). RUN_DIR holds results.csv; validation.json and
# run_record.json are shown when present. --applications renders every
# application in paper/config/applications.json from ESTIMATES_DIR/<id>/.

usage <- paste("Usage: make_explorer.R OUTPUT.html MANIFEST.json RUN_DIR [MANIFEST.json RUN_DIR ...]",
               "| make_explorer.R OUTPUT.html --applications ESTIMATES_DIR [--title TEXT]")
script <- sub("^--file=", "", commandArgs()[grepl("^--file=", commandArgs())][1L])
root <- normalizePath(file.path(dirname(script), "../.."), mustWork = TRUE)
if (dir.exists(file.path(root, "renv/library")) && requireNamespace("renv", quietly = TRUE))
  renv::load(project = root, quiet = TRUE)

abort_if <- function(condition, ...) if (isTRUE(condition)) stop(paste0(...), call. = FALSE)

# Result columns that stay text even when every value looks numeric.
text_columns <- c("spec_id", "panel", "sample_mode", "formula", "estimator", "outcome", "treatment",
                  "control_vars", "fixed_effect_terms", "vcov", "filters", "instruments", "weights",
                  "status", "error", "sample_hash", "dropped_terms", "manifest_hash", "input_hash")

read_results <- function(path, dimensions) {
  abort_if(!file.exists(path), "No results.csv at ", path, ".")
  x <- utils::read.csv(path, colClasses = "character", check.names = FALSE)
  missing <- setdiff(c("spec_id", "status", "estimate"), names(x))
  abort_if(length(missing) > 0L, path, " lacks columns: ", paste(missing, collapse = ", "), ".")
  abort_if(anyDuplicated(x$spec_id) > 0L, path, " repeats specification IDs.")
  # Option IDs stay text ("05" is not 5); other columns become logical or numeric when every value is.
  # R writes TRUE/FALSE and pandas True/False.
  for (column in setdiff(names(x), c(dimensions, text_columns))) {
    value <- x[[column]]
    present <- !is.na(value) & nzchar(value)
    flag <- toupper(value)
    number <- suppressWarnings(as.numeric(value))
    if (!any(present)) x[[column]] <- rep(NA, nrow(x))
    else if (all(flag[present] %in% c("TRUE", "FALSE"))) x[[column]] <- ifelse(present, flag == "TRUE", NA)
    else if (!anyNA(number[present])) x[[column]] <- ifelse(present, number, NA_real_)
  }
  x
}

# Results written without role columns (for example by the Stata executor) are
# marked from the menu, so the published and original specifications still show.
matches_choice <- function(results, choice) {
  keep <- rep(TRUE, nrow(results))
  for (name in intersect(names(choice), names(results))) keep <- keep & results[[name]] == as.character(choice[[name]])
  keep
}

add_roles <- function(results, menu) {
  if (!"preferred" %in% names(results))
    results$preferred <- if (is.null(menu$preferred)) FALSE else matches_choice(results, menu$preferred)
  if (!"original" %in% names(results)) {
    original <- rep(FALSE, nrow(results))
    for (choice in menu$original_specifications) original <- original | matches_choice(results, choice)
    results$original <- original | results$preferred
  }
  results
}

check_complete <- function(results, expected, label) {
  missing <- setdiff(expected, results$spec_id)
  extra <- setdiff(results$spec_id, expected)
  abort_if(length(missing) > 0L, label, " lacks ", length(missing), " frozen specification(s), e.g. ",
           paste(utils::head(missing, 5L), collapse = ", "), ". The explorer shows every row; finish the run first.")
  abort_if(length(extra) > 0L, label, " has specification IDs outside the frozen grid, e.g. ",
           paste(utils::head(extra, 5L), collapse = ", "), ".")
}

read_optional <- function(path) if (file.exists(path)) jsonlite::read_json(path, simplifyVector = FALSE) else NULL

load_application <- function(spec_path, run, context = NULL) {
  abort_if(!file.exists(spec_path), "No manifest or menu at ", spec_path, ".")
  spec <- jsonlite::read_json(spec_path, simplifyVector = FALSE)
  frozen <- !is.null(spec$menu)
  menu <- if (frozen) spec$menu else spec
  abort_if(is.null(menu$version) || is.null(menu$dimensions), spec_path, " is neither a frozen manifest nor a menu.")
  results_path <- if (dir.exists(run)) file.path(run, "results.csv") else run
  run_dir <- dirname(results_path)
  results <- add_roles(read_results(results_path, vapply(menu$dimensions, function(d) d$name, "")), menu)
  label <- file.path(basename(run_dir), basename(results_path))
  manifest <- NULL
  if (frozen) {
    hash <- digest::digest(file = spec_path, algo = "sha256")
    check_complete(results, vapply(spec$grid, function(g) g$spec_id, ""), label)
    recorded <- results$manifest_hash[!is.na(results$manifest_hash) & nzchar(results$manifest_hash)]
    abort_if(length(recorded) > 0L && any(recorded != hash), label, " was not estimated from ",
             basename(spec_path), " (its manifest hash differs).")
    manifest <- list(file = basename(spec_path), sha256 = hash, created_utc = spec$created_utc,
                     expected_rows = spec$expected_rows, input_hash = spec$input_hash,
                     source_hashes = spec$source_hashes, excluded_combinations = length(spec$excluded))
  } else if (file.exists(file.path(run_dir, "grid.csv"))) {
    grid <- utils::read.csv(file.path(run_dir, "grid.csv"), colClasses = "character")
    check_complete(results, grid$spec_id, label)
  } else warning(label, ": no frozen manifest or grid.csv, so completeness was not checked.", call. = FALSE)
  validation <- read_optional(file.path(run_dir, "validation.json"))
  record <- read_optional(file.path(run_dir, "run_record.json"))
  # Hashes, versions, and times only: nothing that names a local path.
  keep <- c("started_utc", "finished_utc", "R_version", "python", "package_versions")
  list(id = menu$version, menu = menu, context = context, manifest = manifest,
       validation = validation[intersect(c("passed", "expected_rows", "completed_rows", "failed_ids",
                                           "baseline_reproduction", "joint_inference"), names(validation))],
       run = record[intersect(keep, names(record))], results_file = label,
       columns = names(results), data = lapply(results, I))
}

paper_applications <- function(estimates) {
  apps <- jsonlite::read_json(file.path(root, "paper/config/applications.json"), simplifyVector = FALSE)$applications
  lapply(apps, function(app) {
    folder <- file.path(estimates, app$id)
    load_application(file.path(folder, paste0("manifest-", app$manifest_version, ".json")),
                     file.path(folder, paste0("run-", app$manifest_version)),
                     context = app[intersect(c("source", "design"), names(app))])
  })
}

html_escape <- function(x) {
  for (pair in list(c("&", "&amp;"), c("<", "&lt;"), c(">", "&gt;"), c("\"", "&quot;")))
    x <- gsub(pair[1L], pair[2L], x, fixed = TRUE)
  x
}

write_explorer <- function(applications, output, title) {
  ids <- vapply(applications, function(a) a$id, "")
  abort_if(anyDuplicated(ids) > 0L, "Two runs share menu version ", ids[duplicated(ids)][1L],
           "; give each its own explorer or version.")
  payload <- list(title = title, generated_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                  generator = "paper/figures/make_explorer.R", applications = applications)
  json <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null", na = "null", digits = NA)
  # No raw "<" inside the data block, so no value can close the script element.
  json <- gsub("<", "\\u003c", json, fixed = TRUE)
  page <- sub("__TITLE__", html_escape(title), explorer_template(), fixed = TRUE)
  parts <- strsplit(page, "__DATA__", fixed = TRUE)[[1L]]
  abort_if(length(parts) != 2L, "The explorer template must contain one data placeholder.")
  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  writeLines(enc2utf8(c(parts[1L], json, parts[2L])), output, sep = "", useBytes = TRUE)
  rows <- sum(vapply(applications, function(a) length(a$data$spec_id), 0L))
  cat(sprintf("Explorer: %s (%d specifications in %d application%s, %.0f KB)\n", output, rows,
              length(applications), if (length(applications) == 1L) "" else "s", file.size(output) / 1024))
}

# The page: plain HTML, CSS, and JavaScript with no external resources.
explorer_template <- function() r"---(<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<meta name="generator" content="make_explorer.R (Customizable-Spec-Curves-R)">
<title>__TITLE__</title>
<style>
:root {
  color-scheme: light dark;
  --bg: #fafaf8; --panel: #ffffff; --surface: #f2f1ee; --border: #dddbd5; --text: #1f1e1c; --muted: #605f5a;
  --accent: #1f6fd1; --baseline: #e0601f; --original: #2a78d6; --other: #55544f; --faint: #d2d0ca;
  --zero: #8c8b86; --grid: #ecebe7; --mark: #3a3935; --hover: #fff1e6; --error: #b3261e;
  --font: system-ui, -apple-system, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif;
  --mono: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
}
@media (prefers-color-scheme: dark) {
  :root {
    --bg: #151514; --panel: #1c1c1a; --surface: #262523; --border: #3b3a37; --text: #ecebe7; --muted: #a8a69f;
    --accent: #7ab0f5; --baseline: #f28a54; --original: #6ea8f2; --other: #b4b2ab; --faint: #46453f;
    --zero: #7c7b75; --grid: #2b2a28; --mark: #dad8d1; --hover: #3a2b20; --error: #f2b8b5;
  }
}
* { box-sizing: border-box; }
html { -webkit-text-size-adjust: 100%; }
body { margin: 0; background: var(--bg); color: var(--text); font: 15px/1.5 var(--font); }
.wrap { max-width: 1320px; margin: 0 auto; padding: 20px 16px 48px; }
h1 { font-size: 1.45rem; line-height: 1.25; margin: 0 0 10px; }
h2 { font-size: 1.15rem; line-height: 1.3; margin: 0 0 4px; }
h3 { font-size: 1rem; margin: 0; }
code { font-family: var(--mono); font-size: .86em; overflow-wrap: anywhere; }
button, select, input { font: inherit; color: inherit; }
button { cursor: pointer; background: var(--panel); border: 1px solid var(--border); border-radius: 6px; padding: 4px 10px; }
button:hover:not(:disabled) { border-color: var(--accent); }
button:disabled { opacity: .45; cursor: default; }
:focus-visible { outline: 2px solid var(--accent); outline-offset: 2px; }
.eyebrow { margin: 0 0 2px; font-size: .78rem; font-weight: 600; letter-spacing: .04em; text-transform: uppercase; color: var(--muted); }
.muted { color: var(--muted); }
.small { font-size: .85em; }
.banner { background: #fff4d6; color: #5c4400; border: 1px solid #e8cf86; border-radius: 8px; padding: 8px 12px; }
@media (prefers-color-scheme: dark) { .banner { background: #3b3018; color: #f3dca0; border-color: #6b5520; } }
#app-picker { display: flex; flex-wrap: wrap; gap: 8px; align-items: center; margin-bottom: 14px; }
#app-picker select { max-width: 100%; padding: 6px 8px; border: 1px solid var(--border); border-radius: 6px; background: var(--panel); }
#app-head { margin-bottom: 14px; }
#app-head p { margin: 2px 0; }
#app-head .estimand { font-size: .95rem; }
#app-head .meta, #app-head .source { font-size: .85rem; color: var(--muted); }
.intro { font-size: .9rem; color: var(--muted); max-width: 75ch; margin: 6px 0 0; }
.layout { display: grid; grid-template-columns: 290px minmax(0, 1fr); gap: 24px; align-items: start; }
aside.filters { position: sticky; top: 12px; max-height: calc(100vh - 24px); overflow: auto; padding-right: 4px; }
@media (max-width: 900px) {
  .layout { grid-template-columns: minmax(0, 1fr); }
  aside.filters { position: static; max-height: none; overflow: visible; }
}
.search label { font-weight: 600; font-size: .9rem; }
.search input { width: 100%; padding: 8px 10px; margin-top: 4px; border: 1px solid var(--border); border-radius: 8px; background: var(--panel); }
.hint { font-size: .78rem; color: var(--muted); margin: 4px 0 10px; }
.toggle { display: flex; gap: 6px; align-items: center; font-size: .88rem; margin: 6px 0; }
.filter-actions { margin: 8px 0 12px; }
details > summary { cursor: pointer; list-style: none; }
details > summary::-webkit-details-marker { display: none; }
details > summary::before { content: "\25B8"; display: inline-block; width: 1em; color: var(--muted); }
details[open] > summary::before { content: "\25BE"; }
details.group { border-top: 1px solid var(--border); padding: 6px 0; }
details.group > summary { display: flex; gap: 2px; font-weight: 600; font-size: .9rem; }
details.group.filtered > summary .glabel { color: var(--accent); }
.gcount { margin-left: auto; font-weight: 400; font-size: .8rem; color: var(--accent); }
details.group .why { font-size: .78rem; color: var(--muted); margin: 2px 0 4px; }
details.group ul { list-style: none; margin: 4px 0 0; padding: 0; }
details.group li { display: flex; align-items: center; gap: 4px; font-size: .86rem; }
details.group li label { flex: 1; display: flex; gap: 6px; align-items: baseline; padding: 2px 0; cursor: pointer; min-width: 0; }
details.group li label .olabel { flex: 1; min-width: 0; overflow-wrap: anywhere; }
details.group li label.zero { color: var(--muted); }
details.group .n { font-size: .78rem; color: var(--muted); font-variant-numeric: tabular-nums; }
details.group button.only { font-size: .72rem; padding: 0 6px; visibility: hidden; }
details.group li:hover button.only, details.group button.only:focus { visibility: visible; }
@media (hover: none) { details.group button.only { visibility: visible; } }
.gbuttons { margin: 2px 0; }
.gbuttons button { font-size: .75rem; padding: 0 8px; }
.legend { display: flex; flex-wrap: wrap; gap: 6px 16px; align-items: center; font-size: .82rem; margin-bottom: 10px; }
.legend span { display: inline-flex; align-items: center; gap: 5px; }
.facet { background: var(--panel); border: 1px solid var(--border); border-radius: 10px; padding: 12px 14px 8px; margin-bottom: 14px; }
.facet-head { display: flex; flex-wrap: wrap; justify-content: space-between; gap: 2px 12px; align-items: baseline; }
.facet-meta, .axis-label { font-size: .8rem; color: var(--muted); margin: 0; }
.stats { display: flex; flex-wrap: wrap; gap: 4px 22px; margin: 8px 0 6px; }
.stats div { min-width: 0; }
.stats dt { font-size: .74rem; color: var(--muted); }
.stats dd { margin: 0; font-weight: 600; font-variant-numeric: tabular-nums; font-size: .95rem; }
.empty { font-size: .9rem; color: var(--muted); }
.chart { width: 100%; overflow: hidden; }
.chart svg { display: block; touch-action: pan-y; }
svg text { font: 10px var(--font); fill: var(--muted); }
.grid { stroke: var(--grid); }
.zero { stroke: var(--zero); }
.ref { stroke: var(--baseline); stroke-dasharray: 4 3; opacity: .85; }
.ci { stroke-width: 1; }
.ci.other { stroke: var(--other); opacity: .3; }
.ci.original { stroke: var(--original); opacity: .6; }
.ci.baseline { stroke: var(--baseline); opacity: .9; }
.pt.other { fill: var(--other); }
.pt.original { fill: var(--original); }
.pt.baseline { fill: var(--baseline); stroke: var(--panel); stroke-width: .8; }
.pt.faint { fill: var(--faint); }
.pt.hollow { fill: none; stroke: var(--baseline); stroke-width: 1.2; }
.stripe { fill: var(--surface); }
.mark { fill: var(--mark); }
.mark.faint { fill: var(--faint); }
.mark.baseline { fill: var(--baseline); }
.dimlabel { font-weight: 600; fill: var(--text); }
.opt { cursor: pointer; }
.opt:hover { fill: var(--accent); text-decoration: underline; }
.opt.solo { fill: var(--accent); font-weight: 600; }
.guide { stroke: var(--accent); opacity: 0; pointer-events: none; }
.guide.on { opacity: .7; }
.overlay { fill: transparent; cursor: crosshair; }
details.medians { margin: 4px 0 4px; font-size: .85rem; }
details.medians summary { color: var(--accent); }
.mtable { border-collapse: collapse; margin-top: 6px; width: 100%; }
.mtable th, .mtable td { text-align: left; vertical-align: top; padding: 4px 8px 4px 0; border-top: 1px solid var(--grid); font-weight: 400; }
.mtable thead th { font-size: .75rem; color: var(--muted); border-top: 0; }
.mtable tbody th { font-weight: 600; white-space: nowrap; }
.mtable .num { text-align: right; font-variant-numeric: tabular-nums; white-space: nowrap; }
.chip { display: inline-block; margin: 0 10px 2px 0; }
.note { font-size: .78rem; color: var(--muted); margin: 4px 0 0; }
.results { margin-top: 18px; }
.toolbar { display: flex; flex-wrap: wrap; justify-content: space-between; gap: 8px; align-items: center; margin-bottom: 8px; }
.toolbar p { margin: 0; font-size: .9rem; }
.toolbar .buttons { display: flex; flex-wrap: wrap; gap: 6px; }
.table-wrap { overflow: auto; max-height: 72vh; border: 1px solid var(--border); border-radius: 10px; background: var(--panel); }
table.specs { border-collapse: collapse; width: 100%; font-size: 13px; font-variant-numeric: tabular-nums; }
table.specs th { position: sticky; top: 0; z-index: 1; background: var(--surface); text-align: left; font-weight: 600; white-space: nowrap; padding: 0; }
table.specs th button { all: unset; cursor: pointer; display: block; padding: 7px 10px; }
table.specs th button:focus-visible { outline: 2px solid var(--accent); }
table.specs td { padding: 5px 10px; border-top: 1px solid var(--grid); white-space: nowrap; }
table.specs .num { text-align: right; }
table.specs tbody tr { cursor: pointer; }
table.specs tbody tr:hover, table.specs tbody tr:focus { background: var(--hover); outline: none; }
table.specs tr.baseline td:first-child { box-shadow: inset 3px 0 0 var(--baseline); }
table.specs tr.original td:first-child { box-shadow: inset 3px 0 0 var(--original); }
table.specs tr.failed td { color: var(--error); }
.sym { display: inline-block; width: 9px; height: 9px; margin-right: 5px; vertical-align: 0; }
.sym.baseline { background: var(--baseline); transform: rotate(45deg) scale(.85); }
.sym.original { width: 0; height: 0; border-left: 5px solid transparent; border-right: 5px solid transparent; border-bottom: 9px solid var(--original); }
.failed-tag { font-weight: 600; }
.more { text-align: center; }
#about { margin-top: 24px; }
#about > details > summary h2 { display: inline; }
#about section { margin-top: 14px; }
#about ul { padding-left: 20px; }
#about li { margin: 2px 0; }
#about .why { font-size: .85rem; color: var(--muted); }
.kv { display: grid; grid-template-columns: minmax(120px, max-content) minmax(0, 1fr); gap: 3px 14px; margin: 6px 0 0; font-size: .9rem; }
.kv dt { color: var(--muted); }
.kv dd { margin: 0; overflow-wrap: anywhere; font-variant-numeric: tabular-nums; }
footer { margin-top: 28px; font-size: .8rem; color: var(--muted); border-top: 1px solid var(--border); padding-top: 10px; }
#tip { position: fixed; z-index: 20; pointer-events: none; max-width: 340px; background: var(--panel); border: 1px solid var(--border); border-radius: 8px; box-shadow: 0 6px 24px rgba(0, 0, 0, .16); padding: 8px 10px; font-size: 12.5px; line-height: 1.4; }
#tip .tip-est { font-variant-numeric: tabular-nums; margin: 2px 0 4px; }
#tip .tip-choices div span { color: var(--muted); }
.tag { display: inline-block; font-size: .72rem; font-weight: 600; border-radius: 99px; padding: 0 8px; border: 1px solid var(--border); margin-right: 4px; }
.tag.baseline { color: var(--baseline); border-color: var(--baseline); }
.tag.original { color: var(--original); border-color: var(--original); }
.tag.failed { color: var(--error); border-color: var(--error); }
dialog { width: min(780px, calc(100vw - 24px)); max-height: calc(100vh - 32px); border: 1px solid var(--border); border-radius: 12px; padding: 0; background: var(--panel); color: var(--text); }
dialog::backdrop { background: rgba(0, 0, 0, .38); }
.dhead { display: flex; justify-content: space-between; gap: 12px; padding: 14px 18px 8px; border-bottom: 1px solid var(--border); position: sticky; top: 0; background: var(--panel); }
.dhead h2 { margin: 0 0 4px; }
.dhead .tags { margin: 0; }
button.icon { font-size: 1.3rem; line-height: 1; padding: 2px 10px; align-self: start; }
.dbody { padding: 6px 18px 12px; }
.dbody h3 { margin-top: 14px; font-size: .9rem; text-transform: uppercase; letter-spacing: .04em; color: var(--muted); }
.dbody .error { color: var(--error); border: 1px solid var(--error); border-radius: 8px; padding: 6px 10px; }
table.choices { border-collapse: collapse; width: 100%; font-size: .9rem; margin-top: 6px; }
table.choices th, table.choices td { text-align: left; vertical-align: top; padding: 4px 10px 4px 0; border-top: 1px solid var(--grid); }
table.choices th { font-weight: 400; color: var(--muted); white-space: nowrap; width: 1%; }
table.choices .why { font-size: .8rem; color: var(--muted); }
.dfoot { display: flex; gap: 8px; align-items: center; padding: 10px 18px 14px; border-top: 1px solid var(--border); position: sticky; bottom: 0; background: var(--panel); }
.dfoot .spacer { flex: 1; }
@media print {
  aside.filters, .toolbar .buttons, .more, #tip, details.medians { display: none !important; }
  .layout { display: block; }
  .table-wrap { max-height: none; overflow: visible; }
}
</style>
</head>
<body>
<div class="wrap">
  <header>
    <p class="eyebrow" id="eyebrow">Specification curve explorer</p>
    <h1 id="page-title"></h1>
    <div id="app-picker"></div>
  </header>
  <noscript><p class="banner">This page needs JavaScript to display the specifications. The same results are distributed as CSV files with the replication code.</p></noscript>
  <section id="app-head" aria-live="polite"></section>
  <div class="layout">
    <aside class="filters" aria-label="Search and filters">
      <div class="search">
        <label for="q">Search all specifications</label>
        <input id="q" type="search" autocomplete="off" spellcheck="false" placeholder="e.g. trend, S0012, -controls">
        <p class="hint">Matches choices, variables, formulas, sample filters, failure messages, and IDs. Every word must match; <code>-word</code> excludes a word; quotes match a phrase.</p>
      </div>
      <label class="toggle"><input id="rerank" type="checkbox"> Re-rank the selection on its own</label>
      <div class="filter-actions"><button type="button" id="reset">Clear search and filters</button></div>
      <div id="filter-groups"></div>
    </aside>
    <main>
      <div id="legend" class="legend"></div>
      <div id="facets"></div>
      <section class="results" aria-label="Specifications">
        <div class="toolbar">
          <p id="count" aria-live="polite"></p>
          <div class="buttons">
            <button type="button" id="download">Download selected rows (CSV)</button>
            <button type="button" id="copy-view">Copy link to this view</button>
          </div>
        </div>
        <div class="table-wrap"><table class="specs" id="table"></table></div>
        <p class="more"><button type="button" id="show-all" hidden></button></p>
      </section>
      <section id="about"></section>
    </main>
  </div>
  <footer id="footer"></footer>
</div>
<div id="tip" role="tooltip" hidden></div>
<dialog id="detail" aria-labelledby="detail-title"></dialog>
<script type="application/json" id="explorer-data">__DATA__</script>
<script>
(() => {
"use strict";
const DATA = JSON.parse(document.getElementById("explorer-data").textContent);
const $ = (s, root = document) => root.querySelector(s);
const ESC = { "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" };
const esc = v => String(v ?? "").replace(/[&<>"']/g, c => ESC[c]);
const isNum = v => typeof v === "number" && Number.isFinite(v);
const str = v => v == null ? "" : String(v);
const ROLES = ["baseline", "original", "other"];
const SHORT = { baseline: "Baseline", original: "Original", other: "" };
const SEARCHED = ["spec_id", "panel", "formula", "estimator", "outcome", "treatment", "control_vars",
  "fixed_effect_terms", "vcov", "filters", "instruments", "weights", "status", "error", "dropped_terms"];
// Fields the detail view shows in its own sections; anything else is listed under "Other fields".
const SHOWN = new Set(["spec_id", "preferred", "original", "panel", "sample_mode", "status", "error", "estimate",
  "std_error", "statistic", "p_value", "ci_lower", "ci_upper", "confidence_level", "first_stage_f", "estimator",
  "formula", "outcome", "treatment", "control_vars", "fixed_effect_terms", "unit_trend", "instruments", "weights",
  "vcov", "filters", "n_input", "n_eligible", "n_obs", "n_units", "n_periods", "n_clusters", "n_estimator_dropped",
  "dropped_terms", "sample_hash", "manifest_hash", "input_hash"]);
const narrow = () => window.matchMedia("(max-width: 900px)").matches;

function fmt(v) {
  if (!isNum(v)) return "–";
  if (v === 0) return "0";
  const a = Math.abs(v);
  if (a >= 1e6 || a < 1e-4) return v.toExponential(2);
  return v.toLocaleString("en-US", { maximumSignificantDigits: 4 });
}
const full = v => isNum(v) ? String(Number(v.toPrecision(8))) : "";
const int = v => isNum(v) ? v.toLocaleString("en-US") : "";
const fmtP = v => !isNum(v) ? "–" : v < 0.001 ? "<0.001" : v.toFixed(3);
const pct = (k, n) => n ? `${Math.round(100 * k / n)}%` : "–";
const cmpStr = (a, b) => a < b ? -1 : a > b ? 1 : 0;
function median(xs) {
  const s = xs.slice().sort((a, b) => a - b), n = s.length;
  return n ? (n % 2 ? s[(n - 1) / 2] : (s[n / 2 - 1] + s[n / 2]) / 2) : NaN;
}
function niceTicks(lo, hi, count) {
  const raw = (hi - lo) / count, mag = Math.pow(10, Math.floor(Math.log10(raw))), err = raw / mag;
  const step = (err >= 7.5 ? 10 : err >= 3.5 ? 5 : err >= 1.5 ? 2 : 1) * mag;
  const ticks = [];
  for (let v = Math.ceil(lo / step) * step; v <= hi + step * 1e-9; v += step) ticks.push(Math.abs(v) < step * 1e-9 ? 0 : v);
  return { ticks, step };
}
function fmtTick(v, step) {
  if (Math.abs(v) < step / 2) return "0";
  const d = Math.max(0, -Math.floor(Math.log10(step) + 1e-9));
  return Math.abs(v) >= 1e6 ? v.toExponential(1) : v.toFixed(Math.min(d, 8));
}

function prepare(a, index) {
  const menu = a.menu || {}, cols = a.columns || [];
  const n = cols.length && a.data[cols[0]] ? a.data[cols[0]].length : 0;
  const rows = [];
  for (let i = 0; i < n; i++) {
    const r = { _i: i };
    for (const c of cols) r[c] = a.data[c] ? a.data[c][i] : null;
    r.spec_id = str(r.spec_id);
    rows.push(r);
  }
  const modes = Array.isArray(menu.sample_modes) ? menu.sample_modes.map(String) : [];
  const multiMode = modes.length > 1 && cols.includes("sample_mode");
  const dims = (menu.dimensions || []).map(d => ({ name: d.name, label: d.label || d.name, why: d.justification || "",
    options: (d.options || []).map(o => ({ id: str(o.id), label: o.label || str(o.id), why: o.justification || "" })) }));
  if (multiMode) dims.push({ name: "sample_mode", label: "Sample", why: "",
    options: modes.map(m => ({ id: m, label: `${m} observations`, why: "" })) });
  for (const d of dims) {
    d.byId = new Map(d.options.map(o => [o.id, o]));
    // A value the menu does not list (possible for non-R results) still gets an option.
    for (const r of rows) {
      const v = str(r[d.name]);
      if (!d.byId.has(v)) { const o = { id: v, label: v || "(blank)", why: "" }; d.options.push(o); d.byId.set(v, o); }
    }
    d.options.forEach((o, k) => { o.k = k; });
  }
  const dimNames = new Set(dims.map(d => d.name));
  // Estimator-specific columns (for example GMM diagnostics) are listed and searched too.
  const extra = cols.filter(c => !SHOWN.has(c) && !dimNames.has(c) && rows.some(r => r[c] != null && r[c] !== ""));
  const panelOrder = [];
  for (const r of rows) {
    r._role = r.preferred === true ? "baseline" : r.original === true ? "original" : "other";
    r._ok = r.status === "ok" && isNum(r.estimate);
    const panel = str(r.panel) || "All specifications";
    if (!panelOrder.includes(panel)) panelOrder.push(panel);
    r._facet = multiMode ? `${panel} (${r.sample_mode} sample)` : panel;
    const parts = [r._role === "other" ? "" : SHORT[r._role]];
    for (const d of dims) { const o = d.byId.get(str(r[d.name])); parts.push(o.id, o.label); }
    for (const c of SEARCHED) if (r[c] != null && r[c] !== "") parts.push(String(r[c]));
    for (const c of extra) if (typeof r[c] === "string" && r[c]) parts.push(r[c]);
    r._text = parts.join(" \u0001 ").toLowerCase();
  }
  const names = [];
  for (const p of panelOrder) for (const m of multiMode ? modes : [null]) {
    const f = m ? `${p} (${m} sample)` : p;
    if (!names.includes(f) && rows.some(r => r._facet === f)) names.push(f);
  }
  const reference = menu.reference && isNum(menu.reference.estimate) ? menu.reference.estimate : null;
  const facets = names.map(name => {
    const all = rows.filter(r => r._facet === name);
    const ok = all.filter(r => r._ok).sort((x, y) => x.estimate - y.estimate || cmpStr(x.spec_id, y.spec_id));
    ok.forEach((r, k) => { r._rank = k + 1; });
    const baseline = all.find(r => r._role === "baseline") || null;
    const ref = baseline && reference !== null ? reference : null;
    // Zero is always on the scale, as in the printed figures. Intervals far
    // longer than the spread of the estimates are clipped so the curve stays legible.
    let elo = Math.min(0, ref ?? 0), ehi = Math.max(0, ref ?? 0), lo = elo, hi = ehi;
    for (const r of ok) {
      elo = Math.min(elo, r.estimate);
      ehi = Math.max(ehi, r.estimate);
      lo = Math.min(lo, r.estimate, isNum(r.ci_lower) ? r.ci_lower : r.estimate);
      hi = Math.max(hi, r.estimate, isNum(r.ci_upper) ? r.ci_upper : r.estimate);
    }
    const span = ehi - elo, clipped = span > 0 && hi - lo > 4 * span;
    if (clipped) { lo = Math.max(lo, elo - span); hi = Math.min(hi, ehi + span); }
    return { name, all, ok, failed: all.filter(r => !r._ok), baseline, ref, lo, hi, clipped };
  });
  const groups = dims.map(d => ({ name: d.name, label: d.label, why: d.why, options: d.options, value: r => str(r[d.name]) }));
  const synthetic = menu.analysis_kind === "synthetic_demo";
  const ap = { index, id: str(a.id), a, menu, rows, dims, facets, groups, synthetic,
    level: isNum(menu.confidence_level) ? menu.confidence_level : 0.95,
    panelDim: menu.panel_by || null, failedAny: rows.some(r => !r._ok),
    facetIndex: new Map(facets.map((f, k) => [f.name, k])), dimNames,
    extra: extra.map(c => ({ key: c, label: c, num: rows.some(r => isNum(r[c])) && rows.every(r => r[c] == null || isNum(r[c])) })) };
  const roles = ROLES.filter(x => rows.some(r => r._role === x));
  if (roles.length > 1) groups.push({ name: "_role", label: "Role", why: "",
    options: roles.map(x => ({ id: x, label: roleLabel(ap, x), why: "" })), value: r => r._role });
  if (ap.failedAny) groups.push({ name: "_status", label: "Status", why: "Failed specifications stay in the results with the reason they failed.",
    options: [{ id: "ok", label: "Estimated", why: "" }, { id: "failed", label: "Failed", why: "" }], value: r => r._ok ? "ok" : "failed" });
  return ap;
}
function roleLabel(ap, role) {
  if (role === "baseline") return ap.synthetic ? "Preferred specification" : "Published baseline";
  return role === "original" ? "Reported in the original article" : "Additional combination";
}

const apps = DATA.applications.map(prepare);
let app = apps[0];
const state = { q: "", off: {}, rerank: false, sel: null, sortKey: null, sortDir: 1, limit: 200, mediansOpen: false };
const view = { sel: new Set(), order: [], charts: [] };

function terms(q) {
  const out = [], re = /(-?)"([^"]*)"|(-?)(\S+)/g;
  let m;
  while ((m = re.exec(q))) {
    const t = (m[2] ?? m[4] ?? "").toLowerCase();
    if (t && t !== "-") out.push({ t, neg: (m[1] || m[3]) === "-" });
  }
  return out;
}
function passes(r, T, skip) {
  for (const g of app.groups) {
    if (g.name === skip) continue;
    const off = state.off[g.name];
    if (off && off.size && off.has(g.value(r))) return false;
  }
  for (const { t, neg } of T) if (r._text.includes(t) === neg) return false;
  return true;
}
function isSolo(name, id) {
  const g = app.groups.find(x => x.name === name), off = state.off[name];
  return !!(g && off && off.size === g.options.length - 1 && !off.has(id));
}
function solo(name, id, toggle) {
  const g = app.groups.find(x => x.name === name);
  if (!g) return;
  if (toggle && isSolo(name, id)) delete state.off[name];
  else state.off[name] = new Set(g.options.map(o => o.id).filter(v => v !== id));
  render();
}

function buildFilters() {
  const closed = narrow();
  $("#filter-groups").innerHTML = app.groups.map(g => `<details class="group" data-group="${esc(g.name)}"${closed || g.options.length > 8 ? "" : " open"}>
    <summary><span class="glabel">${esc(g.label)}</span><span class="gcount"></span></summary>
    ${g.why ? `<p class="why">${esc(g.why)}</p>` : ""}
    <div class="gbuttons"><button type="button" data-all="${esc(g.name)}">Select all</button></div>
    <ul>${g.options.map(o => `<li><label title="${esc(o.why)}"><input type="checkbox" data-group="${esc(g.name)}" value="${esc(o.id)}" checked><span class="olabel">${esc(o.label)}</span><span class="n"></span></label><button type="button" class="only" data-group="${esc(g.name)}" data-only="${esc(o.id)}" aria-label="Show only ${esc(o.label)}">only</button></li>`).join("")}</ul>
  </details>`).join("");
}
function syncFilters(T) {
  const counts = {};
  for (const g of app.groups) {
    const m = new Map();
    for (const r of app.rows) if (passes(r, T, g.name)) { const v = g.value(r); m.set(v, (m.get(v) || 0) + 1); }
    counts[g.name] = m;
  }
  for (const input of document.querySelectorAll("#filter-groups input[type=checkbox]")) {
    const g = input.dataset.group, off = state.off[g], n = counts[g] ? counts[g].get(input.value) || 0 : 0;
    input.checked = !(off && off.has(input.value));
    input.parentElement.querySelector(".n").textContent = n.toLocaleString("en-US");
    input.parentElement.classList.toggle("zero", n === 0);
  }
  for (const det of document.querySelectorAll("#filter-groups details.group")) {
    const g = app.groups.find(x => x.name === det.dataset.group), off = state.off[g.name];
    det.querySelector(".gcount").textContent = off && off.size ? `${g.options.length - off.size} of ${g.options.length}` : "";
    det.classList.toggle("filtered", !!(off && off.size));
  }
}

function render() {
  const T = terms(state.q);
  view.sel = new Set(app.rows.filter(r => passes(r, T)));
  syncFilters(T);
  $("#rerank").checked = state.rerank;
  renderLegend();
  renderFacets();
  renderTable();
  writeHash();
}

function marker(role, x, y, r, extra = "") {
  const c = `pt ${role}${extra}`, X = x.toFixed(1);
  if (role === "baseline") {
    const d = Math.max(3.6, r * 1.9);
    return `<path class="${c}" d="M${X},${(y - d).toFixed(1)}L${(x + d).toFixed(1)},${y.toFixed(1)}L${X},${(y + d).toFixed(1)}L${(x - d).toFixed(1)},${y.toFixed(1)}Z"/>`;
  }
  if (role === "original") {
    const d = Math.max(2.6, r * 1.45);
    return `<path class="${c}" d="M${X},${(y - d).toFixed(1)}L${(x + d).toFixed(1)},${(y + d * .8).toFixed(1)}L${(x - d).toFixed(1)},${(y + d * .8).toFixed(1)}Z"/>`;
  }
  return `<circle class="${c}" cx="${X}" cy="${y.toFixed(1)}" r="${r}"/>`;
}
function renderLegend() {
  const roles = ROLES.filter(x => app.rows.some(r => r._role === x && r._ok));
  const icon = inner => `<svg width="16" height="14" viewBox="0 0 16 14" aria-hidden="true">${inner}</svg>`;
  $("#legend").innerHTML = roles.map(x => `<span>${icon(marker(x, 8, 7, 3))}${esc(roleLabel(app, x))}</span>`).join("")
    + (state.rerank || view.sel.size === app.rows.length ? "" : `<span>${icon('<circle class="pt faint" cx="8" cy="7" r="3"/>')}Not selected</span>`)
    + (app.facets.some(f => f.ref !== null) ? `<span>${icon('<line class="ref" x1="0" x2="16" y1="7" y2="7"/>')}Published estimate</span>` : "")
    + `<span class="muted">Bars: ${Math.round(app.level * 100)}% pointwise intervals</span>`;
}

function renderFacets() {
  const host = $("#facets");
  view.charts = [];
  host.innerHTML = app.facets.map((f, i) => `<section class="facet">
      <div class="facet-head"><h3>${esc(f.name)}</h3><p class="axis-label">${esc(app.menu.estimate_label || "Estimate")}${f.clipped ? "; long intervals are clipped at the plot edges" : ""}</p></div>
      ${statsHTML(f)}<div class="chart" data-facet="${i}"></div>${mediansHTML(f)}</section>`).join("");
  host.querySelectorAll(".chart").forEach(div => {
    const i = +div.dataset.facet;
    div.innerHTML = chartSVG(app.facets[i], i, div.clientWidth || 700);
  });
}
function statsHTML(f) {
  const sel = f.ok.filter(r => view.sel.has(r)), n = sel.length;
  const failedSel = f.failed.filter(r => view.sel.has(r)).length;
  let head = `${n.toLocaleString("en-US")} of ${f.ok.length.toLocaleString("en-US")} estimated specifications selected`;
  if (f.failed.length) head += ` · ${f.failed.length} failed (${failedSel} selected; see the table)`;
  head = `<p class="facet-meta">${head}</p>`;
  if (!n) return head + `<p class="empty">No estimated specification in this panel matches the current search and filters.</p>`;
  const est = sel.map(r => r.estimate);
  const withCI = sel.filter(r => isNum(r.ci_lower) && isNum(r.ci_upper));
  const excl = withCI.filter(r => r.ci_lower > 0 || r.ci_upper < 0).length;
  let base = "";
  if (f.baseline) {
    const b = f.baseline;
    const where = !b._ok ? "failed" : !view.sel.has(b) ? `${fmt(b.estimate)}, not selected`
      : `${fmt(b.estimate)}, rank ${sel.filter(r => r._rank < b._rank).length + 1} of ${n}`;
    base = `<div><dt title="Rank 1 is the lowest estimate in the selection">${esc(roleLabel(app, "baseline"))}</dt><dd>${where}</dd></div>`;
  }
  return head + `<dl class="stats">
      <div><dt>Median</dt><dd>${fmt(median(est))}</dd></div>
      <div><dt>Range</dt><dd>${fmt(Math.min(...est))} to ${fmt(Math.max(...est))}</dd></div>
      <div><dt>Positive</dt><dd>${pct(est.filter(v => v > 0).length, n)}</dd></div>
      <div><dt>Intervals excluding zero</dt><dd>${pct(excl, withCI.length)}</dd></div>${base}</dl>`;
}
function mediansHTML(f) {
  const sel = f.ok.filter(r => view.sel.has(r));
  if (sel.length < 2) return "";
  const rows = [];
  for (const d of app.dims) {
    const by = new Map();
    for (const r of sel) { const v = str(r[d.name]); (by.get(v) || by.set(v, []).get(v)).push(r.estimate); }
    if (by.size < 2) continue;
    const items = d.options.filter(o => by.has(o.id)).map(o => ({ o, n: by.get(o.id).length, m: median(by.get(o.id)) }));
    const ms = items.map(x => x.m);
    rows.push({ d, items, spread: Math.max(...ms) - Math.min(...ms) });
  }
  if (!rows.length) return "";
  rows.sort((a, b) => b.spread - a.spread);
  return `<details class="medians"${state.mediansOpen ? " open" : ""}><summary>Median estimate by choice, largest differences first</summary>
    <table class="mtable"><thead><tr><th>Decision</th><th>Median for each option (number of selected specifications)</th><th class="num">Spread</th></tr></thead><tbody>
    ${rows.map(x => `<tr><th scope="row">${esc(x.d.label)}</th><td>${x.items.map(i => `<span class="chip" title="${esc(i.o.why)}">${esc(i.o.label)}: <b>${fmt(i.m)}</b> <span class="muted">(${i.n})</span></span>`).join("")}</td><td class="num">${fmt(x.spread)}</td></tr>`).join("")}
    </tbody></table><p class="note">Describes the selected specifications in this panel; spread is the largest minus the smallest option median.</p></details>`;
}

function chartSVG(f, i, W) {
  const small = W < 560;
  const ml = small ? 96 : 178, mr = 10, mt = 10, curveH = small ? 150 : 190;
  const pts = state.rerank ? f.ok.filter(r => view.sel.has(r)) : f.ok;
  const N = pts.length, pw = Math.max(40, W - ml - mr), band = pw / Math.max(N, 1);
  const X = k => ml + (k + 0.5) * band;
  let lo = f.lo, hi = f.hi;
  if (hi - lo <= 0) { lo -= 1; hi += 1; }
  const pad = (hi - lo) * 0.06;
  lo -= pad; hi += pad;
  const Y = v => mt + (hi - v) / (hi - lo) * curveH;
  const { ticks, step } = niceTicks(lo, hi, small ? 4 : 5);
  const rowH = 11, headH = 14;
  let y0 = mt + curveH + 16;
  const layout = [];
  for (const d of app.dims) {
    const present = new Set(f.ok.map(r => str(r[d.name])));
    if (present.size < 2) continue;
    const opts = d.options.filter(o => present.has(o.id)).map((o, j) => ({ o, y: y0 + headH + j * rowH }));
    layout.push({ d, head: y0, opts });
    y0 += headH + opts.length * rowH + 6;
  }
  const H = Math.ceil(y0 + 2);
  const r0 = N > 400 ? 1.5 : N > 150 ? 2 : N > 60 ? 2.6 : 3.3;
  const maxChars = Math.max(6, Math.floor((ml - 12) / 5.3));
  const cut = s => s.length > maxChars ? s.slice(0, maxChars - 1) + "…" : s;
  let s = `<svg width="${W}" height="${H}" viewBox="0 0 ${W} ${H}" data-facet="${i}" role="img" aria-label="${esc(`Specification curve, ${f.name}: ${N} estimates sorted from lowest to highest, with the choices behind each`)}">`;
  for (const t of ticks) {
    const y = Y(t);
    s += `<line class="grid" x1="${ml}" x2="${W - mr}" y1="${y.toFixed(1)}" y2="${y.toFixed(1)}"/><text x="${ml - 6}" y="${(y + 3.5).toFixed(1)}" text-anchor="end">${fmtTick(t, step)}</text>`;
  }
  if (lo < 0 && hi > 0) s += `<line class="zero" x1="${ml}" x2="${W - mr}" y1="${Y(0).toFixed(1)}" y2="${Y(0).toFixed(1)}"/>`;
  if (f.ref !== null) s += `<line class="ref" x1="${ml}" x2="${W - mr}" y1="${Y(f.ref).toFixed(1)}" y2="${Y(f.ref).toFixed(1)}"><title>Published estimate: ${esc(fmt(f.ref))}</title></line>`;
  if (!state.rerank) pts.forEach((r, k) => {
    if (view.sel.has(r)) return;
    s += r._role === "baseline" ? marker("baseline", X(k), Y(r.estimate), r0, " hollow") : `<circle class="pt faint" cx="${X(k).toFixed(1)}" cy="${Y(r.estimate).toFixed(1)}" r="${(r0 * .85).toFixed(2)}"/>`;
  });
  for (const role of ["other", "original", "baseline"]) pts.forEach((r, k) => {
    if (r._role !== role || !view.sel.has(r)) return;
    const x = X(k);
    if (isNum(r.ci_lower) && isNum(r.ci_upper)) s += `<line class="ci ${role}" x1="${x.toFixed(1)}" x2="${x.toFixed(1)}" y1="${Y(Math.max(r.ci_lower, lo)).toFixed(1)}" y2="${Y(Math.min(r.ci_upper, hi)).toFixed(1)}"/>`;
    s += marker(role, x, Y(r.estimate), r0);
  });
  const mw = Math.max(1, Math.min(6, band * .75));
  for (const L of layout) {
    s += `<text class="dimlabel" x="2" y="${L.head + 10}">${esc(cut(L.d.label))}<title>${esc(L.d.label)}</title></text>`;
    L.opts.forEach(({ o, y }, j) => {
      if (j % 2 === 0) s += `<rect class="stripe" x="${ml}" y="${y}" width="${pw}" height="${rowH}"/>`;
      const tip = `${o.label}${o.why ? `: ${o.why}` : ""} (click to show only this option; click again for all)`;
      s += `<text class="opt${isSolo(L.d.name, o.id) ? " solo" : ""}" data-dim="${esc(L.d.name)}" data-opt="${esc(o.id)}" x="${ml - 6}" y="${y + 8.5}" text-anchor="end">${esc(cut(o.label))}<title>${esc(tip)}</title></text>`;
    });
    const yOf = new Map(L.opts.map(x => [x.o.id, x.y]));
    pts.forEach((r, k) => {
      const y = yOf.get(str(r[L.d.name]));
      if (y === undefined) return;
      const cls = !view.sel.has(r) ? "mark faint" : r._role === "baseline" ? "mark baseline" : "mark";
      s += `<rect class="${cls}" x="${(X(k) - mw / 2).toFixed(1)}" y="${y + 2}" width="${mw.toFixed(2)}" height="${rowH - 4}"/>`;
    });
  }
  s += `<line class="guide" x1="0" x2="0" y1="${mt}" y2="${H - 2}"/><rect class="overlay" x="${ml}" y="0" width="${pw}" height="${H}"/></svg>`;
  view.charts[i] = { pts, X, band, ml, N };
  return s;
}
function pointAt(e) {
  const ov = e.target.closest && e.target.closest(".overlay");
  if (!ov) return null;
  const svg = ov.ownerSVGElement, c = view.charts[+svg.dataset.facet];
  if (!c || !c.N) return null;
  const x = e.clientX - svg.getBoundingClientRect().left;
  const k = Math.max(0, Math.min(c.N - 1, Math.floor((x - c.ml) / c.band)));
  return { svg, c, k, r: c.pts[k] };
}
function hideTip() {
  $("#tip").hidden = true;
  document.querySelectorAll(".guide.on").forEach(g => g.classList.remove("on"));
}
function showTip(r, cx, cy) {
  const tip = $("#tip");
  const choices = app.dims.filter(d => d.name !== app.panelDim)
    .map(d => `<div><span>${esc(d.label)}:</span> ${esc(d.byId.get(str(r[d.name])).label)}</div>`).join("");
  tip.innerHTML = `<div><b>${esc(r.spec_id)}</b> ${r._role === "other" ? "" : `<span class="tag ${r._role}">${esc(roleLabel(app, r._role))}</span>`}</div>
    <div class="tip-est">${fmt(r.estimate)} <span class="muted">[${fmt(r.ci_lower)}, ${fmt(r.ci_upper)}]</span>${view.sel.has(r) ? "" : ' <span class="muted">not selected</span>'}</div>
    <div class="tip-choices">${choices}</div><div class="muted small">Click for all details</div>`;
  tip.hidden = false;
  const w = tip.offsetWidth, h = tip.offsetHeight;
  let x = cx + 14, y = cy + 14;
  if (x + w > innerWidth - 8) x = cx - w - 14;
  if (y + h > innerHeight - 8) y = cy - h - 14;
  tip.style.left = `${Math.max(8, x)}px`;
  tip.style.top = `${Math.max(8, y)}px`;
}

function tableColumns() {
  const cols = [{ key: "spec_id", label: "Specification" }, { key: "_role", label: "Role" }];
  if (app.facets.length > 1) cols.push({ key: "_facet", label: "Panel" });
  for (const d of app.dims) {
    if (app.facets.length > 1 && (d.name === app.panelDim || d.name === "sample_mode")) continue;
    cols.push({ key: d.name, label: d.label, dim: d });
  }
  cols.push({ key: "estimate", label: "Estimate", num: true }, { key: "std_error", label: "Std. error", num: true },
    { key: "_ci", label: `${Math.round(app.level * 100)}% interval`, num: true }, { key: "p_value", label: "p", num: true },
    { key: "n_obs", label: "Observations", num: true, int: true });
  if (app.rows.some(r => isNum(r.n_clusters))) cols.push({ key: "n_clusters", label: "Clusters", num: true, int: true });
  if (app.rows.some(r => isNum(r.first_stage_f))) cols.push({ key: "first_stage_f", label: "Effective F", num: true });
  cols.push(...app.extra);
  if (app.failedAny) cols.push({ key: "status", label: "Status" });
  return cols;
}
function sortValue(r, key) {
  if (key === "_facet") return app.facetIndex.get(r._facet);
  if (key === "_role") return ROLES.indexOf(r._role);
  if (key === "_ci") return r.ci_lower;
  if (key === "status") return r._ok ? 0 : 1;
  if (app.dimNames.has(key)) return app.dims.find(d => d.name === key).byId.get(str(r[key])).k;
  return r[key];
}
function sortRows(rows) {
  if (!state.sortKey) return rows.slice().sort((a, b) => app.facetIndex.get(a._facet) - app.facetIndex.get(b._facet)
    || (a._ok === b._ok ? 0 : a._ok ? -1 : 1) || (a._rank || 0) - (b._rank || 0) || cmpStr(a.spec_id, b.spec_id));
  const missing = v => v == null || v === "" || (typeof v === "number" && !Number.isFinite(v));
  return rows.slice().sort((a, b) => {
    const x = sortValue(a, state.sortKey), y = sortValue(b, state.sortKey);
    if (missing(x) || missing(y)) return missing(x) === missing(y) ? cmpStr(a.spec_id, b.spec_id) : missing(x) ? 1 : -1;
    const c = typeof x === "number" && typeof y === "number" ? x - y : cmpStr(String(x), String(y));
    return c * state.sortDir || cmpStr(a.spec_id, b.spec_id);
  });
}
function cell(r, c) {
  if (c.key === "spec_id") return `<td>${esc(r.spec_id)}</td>`;
  if (c.key === "_role") return `<td>${r._role === "other" ? "" : `<span class="sym ${r._role}" aria-hidden="true"></span>${SHORT[r._role]}`}</td>`;
  if (c.key === "_facet") return `<td>${esc(r._facet)}</td>`;
  if (c.key === "_ci") return `<td class="num">${isNum(r.ci_lower) && isNum(r.ci_upper) ? `[${fmt(r.ci_lower)}, ${fmt(r.ci_upper)}]` : "–"}</td>`;
  if (c.key === "p_value") return `<td class="num">${fmtP(r.p_value)}</td>`;
  if (c.key === "status") return `<td${r.error ? ` title="${esc(r.error)}"` : ""}>${r._ok ? "estimated" : '<span class="failed-tag">failed</span>'}</td>`;
  if (c.dim) return `<td>${esc(c.dim.byId.get(str(r[c.key])).label)}</td>`;
  if (c.int) return `<td class="num">${int(r[c.key]) || "–"}</td>`;
  if (c.num) return `<td class="num">${fmt(r[c.key])}</td>`;
  return `<td>${esc(r[c.key])}</td>`;
}
function renderTable() {
  const cols = tableColumns();
  view.order = sortRows(app.rows.filter(r => view.sel.has(r)));
  const shown = view.order.slice(0, state.limit);
  $("#count").textContent = `${view.order.length.toLocaleString("en-US")} of ${app.rows.length.toLocaleString("en-US")} specifications selected`
    + (shown.length < view.order.length ? `; the first ${shown.length.toLocaleString("en-US")} are listed` : "");
  const arrow = c => state.sortKey === c.key ? (state.sortDir > 0 ? " ▲" : " ▼") : "";
  const sortAttr = c => state.sortKey === c.key ? ` aria-sort="${state.sortDir > 0 ? "ascending" : "descending"}"` : "";
  $("#table").innerHTML = `<thead><tr>${cols.map(c => `<th scope="col" class="${c.num ? "num" : ""}"${sortAttr(c)}><button type="button" data-sort="${esc(c.key)}" title="Sort">${esc(c.label)}${arrow(c)}</button></th>`).join("")}</tr></thead>`
    + `<tbody>${shown.map(r => `<tr data-i="${r._i}" tabindex="0" class="${r._role}${r._ok ? "" : " failed"}">${cols.map(c => cell(r, c)).join("")}</tr>`).join("")}</tbody>`;
  const more = $("#show-all");
  more.hidden = shown.length >= view.order.length;
  more.textContent = `List all ${view.order.length.toLocaleString("en-US")} selected specifications`;
}

function facetOf(r) { return app.facets[app.facetIndex.get(r._facet)]; }
function detailHTML(r) {
  const k = view.order.indexOf(r), prev = k > 0, next = k >= 0 && k < view.order.length - 1;
  const level = isNum(r.confidence_level) ? r.confidence_level : app.level;
  const text = v => v == null || v === "" ? "" : esc(String(v).replace(/;(?=\S)/g, "; "));
  const code = v => v == null || v === "" ? "" : `<code>${esc(v)}</code>`;
  const dl = pairs => {
    const shown = pairs.filter(p => p[1] !== "");
    return shown.length ? `<dl class="kv">${shown.map(([a, b]) => `<dt>${esc(a)}</dt><dd>${b}</dd>`).join("")}</dl>` : "";
  };
  const part = (title, body) => body ? `<h3>${title}</h3>${body}` : "";
  const choices = app.dims.map(d => {
    const o = d.byId.get(str(r[d.name]));
    return `<tr><th scope="row">${esc(d.label)}</th><td>${esc(o.label)}${o.why ? `<div class="why">${esc(o.why)}</div>` : ""}</td></tr>`;
  }).join("");
  const other = app.extra.map(c => c.key).filter(c => r[c] != null && r[c] !== "");
  const trend = r.unit_trend === true ? "yes" : r.unit_trend === false ? "no" : text(r.unit_trend);
  return `<form method="dialog" class="dhead"><div><p class="eyebrow">${esc(r._facet)}</p>
      <h2 id="detail-title">Specification ${esc(r.spec_id)}</h2>
      <p class="tags">${r._role === "other" ? "" : `<span class="tag ${r._role}">${esc(roleLabel(app, r._role))}</span>`}${r._ok ? "" : '<span class="tag failed">Failed</span>'}${view.sel.has(r) ? "" : '<span class="tag">Not in the current selection</span>'}</p></div>
      <button value="close" class="icon" aria-label="Close">×</button></form>
    <div class="dbody">
      ${r._ok ? "" : `<p class="error"><b>Not estimated:</b> ${esc(r.error || r.status)}</p>`}
      ${part("Estimate", dl([["Estimate", full(r.estimate)], ["Standard error", full(r.std_error)], ["Test statistic", full(r.statistic)],
        ["p-value", full(r.p_value)], [`${Math.round(level * 100)}% pointwise interval`, isNum(r.ci_lower) && isNum(r.ci_upper) ? `[${full(r.ci_lower)}, ${full(r.ci_upper)}]` : ""],
        ["Effective first-stage F", full(r.first_stage_f)], ["Rank in panel (1 = lowest)", r._ok ? `${r._rank} of ${facetOf(r).ok.length}` : ""]]))}
      ${part("Choices", `<table class="choices">${choices}</table>`)}
      ${part("Model", dl([["Estimator", text(r.estimator)], ["Formula", code(r.formula)], ["Outcome", text(r.outcome)], ["Focal variable", text(r.treatment)],
        ["Controls", text(r.control_vars)], ["Fixed effects", text(r.fixed_effect_terms)], ["Unit-specific trends", trend], ["Instruments", text(r.instruments)],
        ["Weights", text(r.weights)], ["Uncertainty", text(r.vcov)], ["Sample filters", text(r.filters)]]))}
      ${part("Sample", dl([["Rows in the input", int(r.n_input)], ["Rows after sample filters", int(r.n_eligible)], ["Observations used", int(r.n_obs)],
        ["Units", int(r.n_units)], ["Periods", int(r.n_periods)], ["Clusters", int(r.n_clusters)], ["Dropped by the estimator", int(r.n_estimator_dropped)],
        ["Dropped terms", text(r.dropped_terms)], ["Observation hash", code(r.sample_hash)]]))}
      ${part("Other fields", dl(other.map(c => [c, typeof r[c] === "number" ? full(r[c]) : text(r[c])])))}
    </div>
    <div class="dfoot"><button type="button" data-nav="-1"${prev ? "" : " disabled"}>← Previous</button>
      <button type="button" data-nav="1"${next ? "" : " disabled"}>Next →</button><span class="spacer"></span>
      <button type="button" data-copy>Copy link to this specification</button></div>`;
}
function openDetail(r) {
  state.sel = r.spec_id;
  const dlg = $("#detail");
  dlg.innerHTML = detailHTML(r);
  if (!dlg.open) { try { dlg.showModal(); } catch (e) { dlg.setAttribute("open", ""); } }
  hideTip();
  writeHash();
}
function closeDetail() { const dlg = $("#detail"); if (dlg.open) dlg.close(); }

function save(blob, name) {
  const a = document.createElement("a");
  a.href = URL.createObjectURL(blob);
  a.download = name;
  document.body.appendChild(a);
  a.click();
  setTimeout(() => { URL.revokeObjectURL(a.href); a.remove(); }, 0);
}
function downloadCSV() {
  const cols = app.a.columns;
  const q = v => {
    if (v == null) return "";
    if (v === true || v === false) return v ? "TRUE" : "FALSE";
    const s = String(v);
    return /[",\r\n]/.test(s) ? `"${s.replace(/"/g, '""')}"` : s;
  };
  const lines = [cols.map(q).join(",")].concat(view.order.map(r => cols.map(c => q(r[c])).join(",")));
  save(new Blob([lines.join("\n") + "\n"], { type: "text/csv" }), `${app.id}-selected-specifications.csv`);
}
async function copyLink(button) {
  writeHash();
  try {
    await navigator.clipboard.writeText(location.href);
    const old = button.textContent;
    button.textContent = "Link copied";
    setTimeout(() => { button.textContent = old; }, 1400);
  } catch (e) { window.prompt("Copy this link:", location.href); }
}

// The address records the view: app, search, hidden or only-shown options, re-ranking, and the open specification.
let ignoreHash = false;
function writeHash() {
  const p = new URLSearchParams();
  if (apps.length > 1) p.set("app", app.id);
  if (state.q) p.set("q", state.q);
  for (const g of app.groups) {
    const off = state.off[g.name];
    if (!off || !off.size) continue;
    const on = g.options.map(o => o.id).filter(v => !off.has(v));
    if (on.length && on.length < off.size) on.forEach(v => p.append(`only.${g.name}`, v));
    else off.forEach(v => p.append(`hide.${g.name}`, v));
  }
  if (state.rerank) p.set("rerank", "1");
  if (state.sel) p.set("spec", state.sel);
  const hash = p.toString() ? `#${p}` : "";
  if (hash === location.hash) return;
  try { history.replaceState(null, "", hash || location.pathname + location.search); }
  catch (e) { ignoreHash = true; location.replace(hash || "#"); }
}
function applyHash(initial) {
  const p = new URLSearchParams(location.hash.slice(1));
  const target = apps.find(x => x.id === p.get("app")) || (initial ? apps[0] : app);
  if (initial || target !== app) { app = target; buildApp(); }
  state.q = p.get("q") || "";
  $("#q").value = state.q;
  state.rerank = p.get("rerank") === "1";
  state.off = {};
  for (const g of app.groups) {
    const ids = g.options.map(o => o.id);
    const only = p.getAll(`only.${g.name}`).filter(v => ids.includes(v));
    const hide = p.getAll(`hide.${g.name}`).filter(v => ids.includes(v));
    if (only.length) state.off[g.name] = new Set(ids.filter(v => !only.includes(v)));
    else if (hide.length) state.off[g.name] = new Set(hide);
  }
  const spec = p.get("spec");
  state.sel = null;
  render();
  const r = spec ? app.rows.find(x => x.spec_id === spec) : null;
  if (r) openDetail(r); else closeDetail();
}

function renderHead() {
  const m = app.menu, a = app.a, v = a.validation || {};
  const failed = app.rows.filter(r => !r._ok).length;
  const meta = [`${app.rows.length.toLocaleString("en-US")} specifications`, failed ? `${failed} failed` : "all estimated"];
  if (v.baseline_reproduction) meta.push(`baseline reproduction ${String(v.baseline_reproduction).replace(/_/g, " ")}`);
  if (a.manifest && a.manifest.created_utc) meta.push(`grid frozen ${String(a.manifest.created_utc).slice(0, 10)}`);
  const context = a.context || {};
  $("#app-head").innerHTML = `${app.synthetic ? '<p class="banner">Generated data: this curve demonstrates the software and is not an empirical result.</p>' : ""}
    <h2>${esc(m.title || app.id)}</h2>
    ${m.estimand ? `<p class="estimand">${esc(m.estimand)}</p>` : ""}
    ${context.design ? `<p class="source">Design: ${esc(context.design)}</p>` : ""}
    ${context.source ? `<p class="source">Source: ${esc(context.source)}</p>` : ""}
    <p class="meta">${meta.map(esc).join(" · ")} · menu <code>${esc(app.id)}</code>${a.manifest ? ` · manifest SHA-256 <code>${esc(String(a.manifest.sha256).slice(0, 12))}…</code>` : ""}</p>
    <p class="intro">Each point is one specification from the frozen menu: one combination of the choices listed under the curve. Points are sorted by estimate within each panel; panels hold different estimands and have their own scales. Search or filter to select specifications: the rest stay grey, so the selection is seen against the whole curve. Click a point or a row for its details, or a choice under the curve to show only that option.</p>`;
}
function renderAbout() {
  const m = app.menu, a = app.a, ref = m.reference || {}, base = m.base || {};
  const value = v => Array.isArray(v) ? v.map(x => typeof x === "object" && x !== null ? JSON.stringify(x) : String(x)).join(", ")
    : typeof v === "object" && v !== null ? Object.entries(v).map(([k, x]) => `${k}: ${Array.isArray(x) ? x.join(", ") : typeof x === "object" && x !== null ? JSON.stringify(x) : x}`).join("; ")
    : String(v);
  const filters = fs => Array.isArray(fs) && fs.length ? fs.map(f => f && f.variable ? `${f.variable} ${f.op} ${Array.isArray(f.value) ? f.value.join(", ") : f.value}` : JSON.stringify(f)).join("; ") : "none";
  const kv = pairs => `<dl class="kv">${pairs.filter(p => p[1] !== "" && p[1] != null).map(([k, v]) => `<dt>${esc(k)}</dt><dd>${v}</dd>`).join("")}</dl>`;
  const dimLabel = name => (app.dims.find(d => d.name === name) || { label: name }).label;
  const optLabel = (name, id) => { const d = app.dims.find(x => x.name === name); const o = d && d.byId.get(str(id)); return o ? o.label : str(id); };
  const preferred = m.preferred || {};
  const b = app.rows.find(r => r._role === "baseline");
  const dims = app.dims.map(d => `<li><b>${esc(d.label)}</b>${d.why ? `<div class="why">${esc(d.why)}</div>` : ""}<ul>${d.options.map(o => `<li>${esc(o.label)} <code>${esc(o.id)}</code>${str(preferred[d.name]) === o.id ? ' <span class="tag baseline">baseline</span>' : ""}${o.why ? `<div class="why">${esc(o.why)}</div>` : ""}</li>`).join("")}</ul></li>`).join("");
  const exclusions = (m.exclusions || []).map(x => `<li>${esc(Object.entries(x.match || {}).map(([k, v]) => `${dimLabel(k)}: ${(Array.isArray(v) ? v : [v]).map(id => optLabel(k, id)).join(" or ")}`).join("; and "))}<div class="why">${esc(x.reason || "")}</div></li>`).join("");
  const man = a.manifest, v = a.validation, run = a.run || {};
  const hashes = man && man.source_hashes ? Object.entries(man.source_hashes).map(([k, h]) => `<div><code>${esc(k)}</code> <code>${esc(h)}</code></div>`).join("") : "";
  const versions = run.package_versions ? Object.entries(run.package_versions).map(([k, x]) => `${k} ${x}`).join(", ") : "";
  $("#about").innerHTML = `<details><summary><h2>About this menu and its provenance</h2></summary>
    <section><h3>Estimand and base model</h3>${kv([["Estimand", esc(m.estimand || "")], ["Estimator", esc(m.estimator || "")],
      ...Object.entries(base).map(([k, x]) => [k.replace(/_/g, " "), esc(k === "filters" ? filters(x) : value(x))]),
      ["Unit; time", esc([m.unit, m.time].filter(Boolean).join("; "))], ["Sample modes", esc((m.sample_modes || []).join(", "))],
      ["Uncertainty", `${Math.round(app.level * 100)}% pointwise intervals; joint inference: ${esc(m.joint_inference || (v && v.joint_inference) || "not reported")}`]])}</section>
    <section><h3>Published baseline</h3>${kv([["Published estimate", full(ref.estimate)], ["Published standard error", full(ref.std_error)],
      ["Tolerance", full(ref.tolerance)], ["Reproduced estimate", b && b._ok ? `${full(b.estimate)} (specification ${esc(b.spec_id)})` : ""],
      ["Source", esc(ref.source || "")]])}</section>
    <section><h3>Decisions and options</h3><ul>${dims}</ul></section>
    ${exclusions ? `<section><h3>Excluded combinations</h3><ul>${exclusions}</ul>${man && man.excluded_combinations ? `<p class="note">${man.excluded_combinations} combination${man.excluded_combinations === 1 ? " is" : "s are"} excluded from the grid.</p>` : ""}</section>` : ""}
    ${m.notes ? `<section><h3>Notes</h3><p>${esc(m.notes)}</p></section>` : ""}
    <section><h3>Provenance</h3>${kv([["Results", `<code>${esc(a.results_file || "")}</code>`],
      ["Frozen manifest", man ? `<code>${esc(man.file)}</code>, frozen ${esc(man.created_utc || "")}` : "none (menu only; the grid was not frozen)"],
      ["Manifest SHA-256", man ? `<code>${esc(man.sha256)}</code>` : ""], ["Input data SHA-256", man && man.input_hash ? `<code>${esc(man.input_hash)}</code>` : ""],
      ["Analysis source SHA-256", hashes],
      ["Validation", v && v.passed != null ? esc(`${v.passed ? "passed" : "failed"}; ${v.completed_rows} of ${v.expected_rows} rows completed; baseline reproduction ${v.baseline_reproduction || "not recorded"}`) : ""],
      ["Run", esc([run.started_utc && `started ${run.started_utc}`, run.finished_utc && `finished ${run.finished_utc}`, run.R_version, run.python && `Python ${run.python}`].filter(Boolean).join("; "))],
      ["Packages", esc(versions)]])}
      <p><button type="button" id="menu-json">Download the menu (JSON)</button></p></section>
  </details>`;
  $("#menu-json").addEventListener("click", () => save(new Blob([JSON.stringify(m, null, 2) + "\n"], { type: "application/json" }), `${app.id}-menu.json`));
}
function renderPicker() {
  const host = $("#app-picker");
  if (apps.length < 2) { host.innerHTML = ""; return; }
  host.innerHTML = `<label for="app-select">Application</label><select id="app-select">${apps.map(x => `<option value="${esc(x.id)}"${x === app ? " selected" : ""}>${esc(x.menu.title || x.id)} (${x.rows.length.toLocaleString("en-US")} specifications)</option>`).join("")}</select>`;
  $("#app-select").addEventListener("change", e => {
    app = apps.find(x => x.id === e.target.value) || app;
    buildApp();
    Object.assign(state, { q: "", off: {}, rerank: false, sel: null });
    $("#q").value = "";
    closeDetail();
    render();
  });
}
function buildApp() {
  Object.assign(state, { sortKey: null, sortDir: 1, limit: 200 });
  buildFilters();
  renderPicker();
  renderHead();
  renderAbout();
}

$("#page-title").textContent = DATA.title || "Specification curve explorer";
$("#eyebrow").hidden = $("#page-title").textContent === $("#eyebrow").textContent;
$("#footer").innerHTML = `Generated ${esc(DATA.generated_utc || "")} by <code>${esc(DATA.generator || "make_explorer.R")}</code> from frozen menus and validated results; nothing on this page is estimated in the browser. Intervals are pointwise. Medians, ranges, and shares describe the selected specifications; they are not hypothesis tests, and no joint inference is reported.`;
let searchTimer;
$("#q").addEventListener("input", e => {
  clearTimeout(searchTimer);
  searchTimer = setTimeout(() => { state.q = e.target.value.trim(); render(); }, 120);
});
$("#rerank").addEventListener("change", e => { state.rerank = e.target.checked; render(); });
$("#reset").addEventListener("click", () => { state.q = ""; $("#q").value = ""; state.off = {}; state.rerank = false; render(); });
$("#download").addEventListener("click", downloadCSV);
$("#copy-view").addEventListener("click", e => copyLink(e.currentTarget));
$("#show-all").addEventListener("click", () => { state.limit = Infinity; renderTable(); });
$("#filter-groups").addEventListener("change", e => {
  const input = e.target;
  if (!input.matches("input[type=checkbox]")) return;
  const g = input.dataset.group, set = state.off[g] || (state.off[g] = new Set());
  if (input.checked) set.delete(input.value); else set.add(input.value);
  if (!set.size) delete state.off[g];
  render();
});
$("#filter-groups").addEventListener("click", e => {
  const b = e.target.closest("button");
  if (!b) return;
  if (b.dataset.all) { delete state.off[b.dataset.all]; render(); }
  else if (b.dataset.only !== undefined) solo(b.dataset.group, b.dataset.only, false);
});
const facetsHost = $("#facets");
facetsHost.addEventListener("mousemove", e => {
  const hit = pointAt(e);
  if (!hit) { hideTip(); return; }
  const guide = hit.svg.querySelector(".guide"), x = hit.c.X(hit.k).toFixed(1);
  document.querySelectorAll(".guide.on").forEach(g => { if (g !== guide) g.classList.remove("on"); });
  guide.setAttribute("x1", x);
  guide.setAttribute("x2", x);
  guide.classList.add("on");
  showTip(hit.r, e.clientX, e.clientY);
});
facetsHost.addEventListener("mouseleave", hideTip);
facetsHost.addEventListener("click", e => {
  const opt = e.target.closest("text.opt");
  if (opt) { solo(opt.dataset.dim, opt.dataset.opt, true); return; }
  const hit = pointAt(e);
  if (hit) openDetail(hit.r);
});
facetsHost.addEventListener("toggle", e => { if (e.target.matches("details.medians")) state.mediansOpen = e.target.open; }, true);
$("#table").addEventListener("click", e => {
  const b = e.target.closest("button[data-sort]");
  if (b) {
    const k = b.dataset.sort;
    if (state.sortKey !== k) { state.sortKey = k; state.sortDir = 1; }
    else if (state.sortDir > 0) state.sortDir = -1;
    else state.sortKey = null;
    renderTable();
    return;
  }
  const tr = e.target.closest("tr[data-i]");
  if (tr) openDetail(app.rows[+tr.dataset.i]);
});
$("#table").addEventListener("keydown", e => {
  const tr = e.target.closest("tr[data-i]");
  if (tr && e.key === "Enter") { e.preventDefault(); openDetail(app.rows[+tr.dataset.i]); }
});
const dialog = $("#detail");
function step(d) {
  const k = view.order.findIndex(r => r.spec_id === state.sel), r = view.order[k + d];
  if (k >= 0 && r) openDetail(r);
}
dialog.addEventListener("click", e => {
  if (e.target === dialog) { closeDetail(); return; }
  const nav = e.target.closest("button[data-nav]");
  if (nav) step(+nav.dataset.nav);
  else if (e.target.closest("button[data-copy]")) copyLink(e.target.closest("button[data-copy]"));
});
dialog.addEventListener("keydown", e => {
  if (e.key === "ArrowLeft") step(-1);
  else if (e.key === "ArrowRight") step(1);
});
dialog.addEventListener("close", () => { state.sel = null; writeHash(); });
window.addEventListener("hashchange", () => { if (ignoreHash) { ignoreHash = false; return; } applyHash(false); });
let lastWidth = 0, resizeTimer;
new ResizeObserver(() => {
  const w = facetsHost.clientWidth;
  if (Math.abs(w - lastWidth) < 2) return;
  lastWidth = w;
  clearTimeout(resizeTimer);
  resizeTimer = setTimeout(renderFacets, 80);
}).observe(facetsHost);
applyHash(true);
})();
</script>
</body>
</html>
)---"

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  title <- "Specification curve explorer"
  at <- match("--title", args)
  if (!is.na(at)) {
    abort_if(at == length(args), usage)
    title <- args[at + 1L]
    args <- args[-c(at, at + 1L)]
  }
  abort_if(length(args) < 3L, usage)
  rest <- args[-1L]
  applications <- if (identical(rest[1L], "--applications")) {
    abort_if(length(rest) != 2L, usage)
    paper_applications(rest[2L])
  } else {
    abort_if(length(rest) %% 2L != 0L, usage)
    lapply(seq(1L, length(rest), by = 2L), function(i) load_application(rest[i], rest[i + 1L]))
  }
  write_explorer(applications, args[1L], title)
}
