#!/usr/bin/env Rscript
# Every number cited in the manuscript's application section, computed from
# validated results only. Writes a JSON file the manuscript text is checked against.
#
# Usage: Rscript paper/figures/paper_numbers.R ESTIMATES_DIR OUTPUT.json

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("Usage: paper_numbers.R ESTIMATES_DIR OUTPUT.json")
script <- sub("^--file=", "", commandArgs()[grepl("^--file=", commandArgs())][1L])
root <- normalizePath(file.path(dirname(script), "../.."), mustWork = TRUE)
apps <- jsonlite::read_json(file.path(root, "paper/config/applications.json"))$applications
read_results <- function(id) {
  app <- Filter(function(a) identical(a$id, id), apps)[[1L]]
  x <- utils::read.csv(file.path(args[1L], id, paste0("run-", app$manifest_version), "results.csv"), stringsAsFactors = FALSE)
  stopifnot(all(x$status == "ok"))
  x
}
bll <- read_results("bll2010")
cz <- read_results("cz_am")
bliq <- read_results("cz_bliq")
btmr <- read_results("btmr2021")
excludes_zero <- function(x) x$ci_lower > 0 | x$ci_upper < 0

describe <- function(x) {
  base <- x[x$preferred, ]
  list(n = nrow(x), median = stats::median(x$estimate), min = min(x$estimate), max = max(x$estimate),
       n_positive = sum(x$estimate > 0), n_excl_zero = sum(excludes_zero(x)),
       n_excl_zero_negative = sum(excludes_zero(x) & x$estimate < 0),
       n_excl_zero_positive = sum(excludes_zero(x) & x$estimate > 0),
       baseline = if (nrow(base)) base$estimate else NULL,
       baseline_se = if (nrow(base)) base$std_error else NULL,
       baseline_rank_from_bottom = if (nrow(base)) sum(x$estimate <= base$estimate) else NULL)
}
by_option <- function(x, dimension) lapply(split(x, x[[dimension]]), function(g)
  list(n = nrow(g), median = stats::median(g$estimate), n_excl_zero = sum(excludes_zero(g)),
       min = min(g$estimate), max = max(g$estimate)))

numbers <- list(bll = list(), cz = list(), bliq = list())
for (panel in unique(bll$panel)) {
  x <- bll[bll$panel == panel, ]
  original <- x[x$original, ]
  numbers$bll[[panel]] <- c(describe(x), list(
    original = list(n = nrow(original), min = min(original$estimate), max = max(original$estimate),
                    n_excl_zero = sum(excludes_zero(original))),
    by = lapply(stats::setNames(nm = c("controls", "window", "states", "time_effects", "timing")),
                function(d) by_option(x, d))))
  # The published combination's choices: count how many dimensions the baseline
  # sits at the most negative option median.
}
log_gini <- bll[bll$panel == "Log Gini", ]
year_fe <- log_gini[log_gini$time_effects == "year", ]
numbers$bll$log_gini_year_fe_no_controls_full <- describe(
  year_fe[year_fe$controls == "none" & year_fe$window == "y2006", ])
numbers$bll$log_gini_no_year_fe_excl_zero <- sum(excludes_zero(log_gini[log_gini$time_effects != "year", ]))
numbers$bll$log_gini_no_year_fe_n <- sum(log_gini$time_effects != "year")
leading_year_only <- function(x) { x <- x[order(x$estimate), ]; k <- which(x$time_effects != "year")[1L] - 1L; k }
numbers$bll$leading_year_effects_only <- lapply(split(bll, bll$panel), leading_year_only)
base <- log_gini$estimate[log_gini$preferred]
numbers$bll$log_gini_baseline_minus_minimum <- base - min(log_gini$estimate)
numbers$bll$log_gini_baseline_percent <- 100 * (exp(base) - 1)
numbers$bll$log_gini_median_percent <- 100 * (exp(stats::median(log_gini$estimate)) - 1)

for (panel in unique(cz$panel)) {
  x <- cz[cz$panel == panel, ]
  ew_none <- x[x$weighting == "ew" & x$screen == "none", ]
  vw <- x[x$weighting == "vw", ]
  screened <- x[x$screen != "none", ]
  numbers$cz[[panel]] <- c(describe(x), list(
    ew_unscreened = list(n = nrow(ew_none), min = min(ew_none$estimate), max = max(ew_none$estimate),
                         n_excl_zero = sum(excludes_zero(ew_none))),
    value_weighted = list(n = nrow(vw), min = min(vw$estimate), max = max(vw$estimate),
                          n_excl_zero = sum(excludes_zero(vw))),
    screened = lapply(split(screened, screened$screen), function(g)
      list(estimate = g$estimate, excl_zero = excludes_zero(g))),
    constructions = stats::setNames(lapply(seq_len(nrow(x)), function(i) list(
      estimate = x$estimate[i], se = x$std_error[i], ci = c(x$ci_lower[i], x$ci_upper[i]), n_obs = x$n_obs[i])),
      paste(x$breakpoints, x$weighting, x$rebalancing, x$screen, sep = "/"))))
}
op <- cz[cz$preferred, ]
numbers$cz$baseline_newey_west_t <- op$statistic

preferred <- bliq[bliq$preferred, ]
for (panel in unique(bliq$panel)) {
  x <- bliq[bliq$panel == panel, ]
  published <- x[x$breakpoints == preferred$breakpoints & x$weighting == preferred$weighting &
                   x$rebalancing == preferred$rebalancing & x$screen == preferred$screen, ]
  ew <- x[x$weighting == "ew", ]
  numbers$bliq[[panel]] <- c(describe(x), list(
    published_construction = published$estimate,
    published_rank_from_top = sum(x$estimate >= published$estimate),
    others_min = min(x$estimate[x$spec_id != published$spec_id]),
    others_max = max(x$estimate[x$spec_id != published$spec_id]),
    equal_weighted = list(n = nrow(ew), min = min(ew$estimate), max = max(ew$estimate)),
    constructions = stats::setNames(lapply(seq_len(nrow(x)), function(i) list(
      estimate = x$estimate[i], ci = c(x$ci_lower[i], x$ci_upper[i]), n_obs = x$n_obs[i])),
      paste(x$breakpoints, x$weighting, x$rebalancing, x$screen, sep = "/"))))
}
numbers$bliq$baseline_newey_west_t <- preferred$statistic

numbers$btmr <- c(describe(btmr), list(
  rank_from_top = lapply(split(btmr[btmr$original, ], seq_len(sum(btmr$original))), function(r)
    list(choice = paste(r$macro, r$city_size), estimate = r$estimate, rank = sum(btmr$estimate >= r$estimate),
         excludes_zero = r$ci_lower > 0 | r$ci_upper < 0)),
  by = lapply(stats::setNames(nm = c("macro", "city_size", "time_effects", "sample", "outcome_def")),
              function(d) by_option(btmr, d))))
jsonlite::write_json(numbers, args[2L], pretty = TRUE, auto_unbox = TRUE, digits = 6)
cat("Wrote", args[2L], "\n")
