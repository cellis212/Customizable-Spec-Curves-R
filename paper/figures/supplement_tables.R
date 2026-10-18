#!/usr/bin/env Rscript
# LaTeX tables for the Supplementary Material, generated from the frozen menus
# and validated results (nothing is estimated here).
#
# Usage: Rscript paper/figures/supplement_tables.R ESTIMATES_DIR OUTPUT_DIR

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("Usage: supplement_tables.R ESTIMATES_DIR OUTPUT_DIR")
script <- sub("^--file=", "", commandArgs()[grepl("^--file=", commandArgs())][1L])
root <- normalizePath(file.path(dirname(script), "../.."), mustWork = TRUE)
dir.create(args[2L], recursive = TRUE, showWarnings = FALSE)
apps <- jsonlite::read_json(file.path(root, "paper/config/applications.json"))$applications
paths <- function(id) {
  app <- Filter(function(a) identical(a$id, id), apps)[[1L]]
  list(manifest = file.path(args[1L], id, paste0("manifest-", app$manifest_version, ".json")),
       results = file.path(args[1L], id, paste0("run-", app$manifest_version), "results.csv"))
}
escape <- function(x) {
  x <- gsub("([&%$#_{}])", "\\\\\\1", x)
  gsub("([0-9]{4})-([0-9]{4})", "\\1--\\2", x)
}
read_menu <- function(id) jsonlite::read_json(paths(id)$manifest, simplifyVector = FALSE)$menu
option_label <- function(menu, dim, id) {
  d <- Filter(function(x) x$name == dim, menu$dimensions)[[1L]]
  Filter(function(o) o$id == id, d$options)[[1L]]$label
}

menu_table <- function(id, path) {
  menu <- read_menu(id)
  lines <- c("\\footnotesize", paste0("\\begin{tabular}{@{}>{\\raggedright\\arraybackslash}p{0.16\\linewidth}",
             ">{\\raggedright\\arraybackslash}p{0.31\\linewidth}>{\\raggedright\\arraybackslash}p{0.47\\linewidth}@{}}"),
             "\\toprule", "Decision & Option & Justification \\\\", "\\midrule")
  for (d in menu$dimensions) {
    first <- TRUE
    previous <- NULL
    for (o in d$options) {
      label <- if (identical(o$id, menu$preferred[[d$name]])) paste0(escape(o$label), "$^{\\ast}$") else escape(o$label)
      why <- if (is.null(o$justification)) "" else o$justification
      # A run of options with the same justification (e.g. leave one state out) prints it once.
      shown <- if (identical(why, previous)) "" else why
      lines <- c(lines, sprintf("%s & %s & %s \\\\", if (first) escape(d$label) else "", label, escape(shown)))
      previous <- why
      first <- FALSE
    }
    lines <- c(lines, "\\addlinespace")
  }
  writeLines(c(lines, "\\bottomrule", "\\end{tabular}"), path)
}

portfolio_results_table <- function(id, path) {
  menu <- read_menu(id)
  x <- utils::read.csv(paths(id)$results, stringsAsFactors = FALSE)
  periods <- vapply(Filter(function(d) d$name == "period", menu$dimensions)[[1L]]$options, function(o) o$id, "")
  x <- x[order(match(x$period, periods), -x$estimate), ]
  lines <- c("\\footnotesize", "\\begin{tabular}{@{}lllllrrrr@{}}", "\\toprule",
             "Period & Breakpoints & Weighting & Rebalancing & Screen & Mean & s.e. & 95\\% CI & Months \\\\", "\\midrule")
  for (i in seq_len(nrow(x))) {
    period <- option_label(menu, "period", x$period[i])
    lines <- c(lines, sprintf("%s & %s & %s & %s & %s & $%.3f$ & $%.3f$ & $[%.2f, %.2f]$ & %d \\\\",
      escape(sub(" .*", "", period)), option_label(menu, "breakpoints", x$breakpoints[i]),
      option_label(menu, "weighting", x$weighting[i]), option_label(menu, "rebalancing", x$rebalancing[i]),
      escape(option_label(menu, "screen", x$screen[i])), x$estimate[i], x$std_error[i], x$ci_lower[i],
      x$ci_upper[i], x$n_obs[i]))
    if (i == sum(x$period == periods[1L])) lines <- c(lines, "\\addlinespace")
  }
  writeLines(c(lines, "\\bottomrule", "\\end{tabular}"), path)
}

bll_options_table <- function(path) {
  menu <- read_menu("bll2010")
  bll <- utils::read.csv(paths("bll2010")$results, stringsAsFactors = FALSE)
  panels <- c("Log Gini", "Logistic Gini", "Log Theil")
  excl <- function(x) sum(x$ci_lower > 0 | x$ci_upper < 0)
  cell <- function(g) sprintf("$%.4f$ & %d/%d", stats::median(g$estimate), excl(g), nrow(g))
  lines <- c("\\footnotesize", "\\begin{tabular}{@{}llrrrrrr@{}}", "\\toprule",
             " & & \\multicolumn{2}{c}{Log Gini} & \\multicolumn{2}{c}{Logistic Gini} & \\multicolumn{2}{c}{Log Theil} \\\\",
             "\\cmidrule(lr){3-4}\\cmidrule(lr){5-6}\\cmidrule(l){7-8}",
             "Decision & Option & Median & Excl.\\ 0 & Median & Excl.\\ 0 & Median & Excl.\\ 0 \\\\", "\\midrule")
  for (d in menu$dimensions) {
    if (d$name == menu$panel_by) next
    first <- TRUE
    for (o in d$options) {
      cells <- vapply(panels, function(p) cell(bll[bll$panel == p & bll[[d$name]] == o$id, ]), "")
      lines <- c(lines, sprintf("%s & %s & %s \\\\", if (first) escape(d$label) else "", escape(o$label),
                                paste(cells, collapse = " & ")))
      first <- FALSE
    }
    lines <- c(lines, "\\addlinespace")
  }
  all_cells <- vapply(panels, function(p) cell(bll[bll$panel == p, ]), "")
  writeLines(c(lines, sprintf("All & & %s \\\\", paste(all_cells, collapse = " & ")), "\\bottomrule", "\\end{tabular}"), path)
}

out <- args[2L]
menu_table("bll2010", file.path(out, "table_menu_bll.tex"))
menu_table("cz_am", file.path(out, "table_menu_cz.tex"))
menu_table("cz_bliq", file.path(out, "table_menu_bliq.tex"))
menu_table("btmr2021", file.path(out, "table_menu_btmr.tex"))
bll_options_table(file.path(out, "table_options_bll.tex"))
portfolio_results_table("cz_am", file.path(out, "table_results_cz.tex"))
portfolio_results_table("cz_bliq", file.path(out, "table_results_bliq.tex"))
menu_table("fi2015", file.path(out, "table_menu_fi.tex"))
menu_table("cfr2010", file.path(out, "table_menu_cfr.tex"))
menu_table("bp2005", file.path(out, "table_menu_bp.tex"))
cat("Supplement tables written to", normalizePath(out), "\n")
