args <- commandArgs()
script <- sub("^--file=", "", args[grepl("^--file=", args)][1L])
root <- normalizePath(file.path(dirname(script), "../.."))
if (dir.exists(file.path(root, "renv/library")) && requireNamespace("renv", quietly = TRUE))
  renv::load(project = root, quiet = TRUE)
for (file in c("curve.R", "demo.R", "plot.R")) source(file.path(root, "paper/R", file))

checks <- 0L
check <- function(condition, message) {
  checks <<- checks + 1L
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}
expect_error <- function(expr, pattern) {
  error <- tryCatch({force(expr); NULL}, error = conditionMessage)
  check(!is.null(error) && grepl(pattern, error), paste("Expected error:", pattern))
}

directory <- tempfile("spec-curve-test-")
dir.create(directory)
menu <- read_menu(file.path(root, "paper/config/demo.json"))
data <- make_demo_data(menu$seed)
path <- file.path(directory, "manifest.json")
manifest <- freeze_manifest(menu, data, root, path)
results <- run_manifest(manifest, data)
validation <- validate_results(manifest, results)
check(validation$passed, "Synthetic grid should complete.")
check(nrow(results) == 6L && all(results$status == "ok"), "All six planned rows must exist.")
check(all(grepl("required_control", results$formula)), "Required controls must never become optional.")
check(all(results$vcov == "cluster(firm)"), "Clustering must remain fixed.")
common <- results[results$sample_mode == "common", ]
check(length(unique(common$sample_hash)) == 1L, "Common-sample rows must share observation identities.")
available <- results[results$sample_mode == "available", ]
check(length(unique(available$n_obs)) > 1L, "Available-sample diagnostics should detect attrition.")

preferred <- results[results$preferred, ]
direct <- fixest::feols(y ~ x + required_control | firm + year, data = data, vcov = ~firm,
                        ssc = fixest::ssc(K.adj = TRUE, K.fixef = "nonnested", G.adj = TRUE, t.df = "min"),
                        notes = FALSE, nthreads = 1L)
check(abs(preferred$estimate - unname(stats::coef(direct)["x"])) < 1e-12,
      "Preferred estimate must match an independent direct call.")
expect_error(freeze_manifest(menu, data, root, path), "already exists")
expect_error(validate_results(manifest, results[-1L, ]), "exactly one row")
expect_error(validate_results(manifest, rbind(results, results[1L, ])), "exactly one row")
changed <- data
changed$y[1L] <- changed$y[1L] + 1
expect_error(read_manifest(path, changed, root), "Input data differ")

zero <- results
zero$estimate[1L] <- 0
zero$ci_lower[1L] <- -1
zero$ci_upper[1L] <- 1
check(validate_results(manifest, zero)$passed, "A zero coefficient is a valid result, not a missing row.")
wrong_labels <- results
wrong_labels$controls[1L] <- "unexpected"
expect_error(validate_results(manifest, wrong_labels), "Specification labels changed")

application <- menu
application$analysis_kind <- "application"
expect_error(validate_menu(application, data), "published baseline")
application$reference <- list(estimate = preferred$estimate, tolerance = 1e-10, source = "Independent direct-model test")
application_manifest <- freeze_manifest(application, data, root, file.path(directory, "application.json"))
application_results <- run_manifest(application_manifest, data)
check(validate_results(application_manifest, application_results)$baseline_reproduction == "passed",
      "Application baseline must be checked against the documented reference.")
application_manifest$menu$reference$estimate <- 100
expect_error(validate_results(application_manifest, application_results), "published baseline")

unidentified <- data
unidentified$x <- 0
bad_manifest <- freeze_manifest(menu, unidentified, root, file.path(directory, "unidentified.json"))
bad_results <- suppressWarnings(run_manifest(bad_manifest, unidentified))
check(all(bad_results$status == "failed") && all(nzchar(bad_results$error)),
      "Unidentified focal coefficients must retain failed rows and explanations.")
check(!validate_results(bad_manifest, bad_results)$passed, "Failed grids must block plotting.")
absorbed <- data
absorbed$required_control <- 1
absorbed_manifest <- freeze_manifest(menu, absorbed, root, file.path(directory, "absorbed.json"))
absorbed_results <- suppressWarnings(run_manifest(absorbed_manifest, absorbed))
check(all(absorbed_results$status == "failed") && all(grepl("required control", absorbed_results$error)),
      "Absorbed required controls must not silently disappear.")
missing_provenance <- results
missing_provenance$manifest_hash[1L] <- NA_character_
expect_error(validate_results(manifest, missing_provenance), "must be nonmissing")


# Declarative dimensions: filters, exclusions, estimand panels, and unit trends.
rich <- menu
rich$dimensions <- list(
  list(name = "controls", label = "Controls", options = list(
    list(id = "required", label = "Required only"),
    list(id = "plus_size", label = "+ size", add_controls = list("size")))),
  list(name = "window", label = "Window", options = list(
    list(id = "all", label = "2001-2012"),
    list(id = "late", label = "2004-2012", filters = list(list(variable = "year", op = ">=", value = 2004))))),
  list(name = "trend", label = "Trends", options = list(
    list(id = "none", label = "None", unit_trend = FALSE),
    list(id = "firm", label = "Firm trends", unit_trend = TRUE))),
  list(name = "outcome_def", label = "Outcome", options = list(
    list(id = "level", label = "Level", outcome = "y"),
    list(id = "scaled", label = "Scaled", outcome = "y_scaled"))))
rich$panel_by <- "outcome_def"
rich$sample_modes <- list("available")
rich$preferred <- list(controls = "required", window = "all", trend = "none", outcome_def = "level", sample_mode = "available")
rich$exclusions <- list(list(match = list(trend = "firm", window = "late"), reason = "Too few years for firm trends."))
rich$original_specifications <- list(list(controls = "plus_size", window = "all", trend = "none", outcome_def = "level",
                                          sample_mode = "available"))
rich_data <- data
rich_data$y_scaled <- 100 * rich_data$y
rich_manifest <- freeze_manifest(rich, rich_data, root, file.path(directory, "rich.json"))
check(rich_manifest$expected_rows == 12L && nrow(rich_manifest$excluded) == 4L,
      "Exclusions must remove declared combinations and be recorded.")
check(sum(rich_manifest$grid$original) == 2L, "Original specifications include the baseline and declared rows.")
rich_results <- run_manifest(rich_manifest, rich_data)
check(validate_results(rich_manifest, rich_results)$passed, "Rich grid should complete.")
late <- rich_results[rich_results$window == "late" & rich_results$controls == "required" &
                       rich_results$outcome_def == "level", ]
direct_late <- fixest::feols(y ~ x + required_control | firm + year, data = data[data$year >= 2004, ], vcov = ~firm,
                             ssc = fixest::ssc(K.adj = TRUE, K.fixef = "nonnested", G.adj = TRUE, t.df = "min"),
                             notes = FALSE, nthreads = 1L)
check(abs(late$estimate - unname(stats::coef(direct_late)["x"])) < 1e-12, "Filters must match a direct subset.")
trend <- rich_results[rich_results$trend == "firm" & rich_results$controls == "required" &
                        rich_results$outcome_def == "level", ]
direct_trend <- fixest::feols(y ~ x + required_control | firm[year] + year, data = data, vcov = ~firm,
                              ssc = fixest::ssc(K.adj = TRUE, K.fixef = "nonnested", G.adj = TRUE, t.df = "min"),
                              notes = FALSE, nthreads = 1L)
check(abs(trend$estimate - unname(stats::coef(direct_trend)["x"])) < 1e-12, "Unit trends must match a direct call.")
scaled <- rich_results[rich_results$outcome_def == "scaled", ]
level <- rich_results[rich_results$outcome_def == "level", ]
check(all(abs(scaled$estimate[order(scaled$spec_id)] - 100 * level$estimate[order(level$spec_id)]) < 1e-8),
      "Outcome options must change only the outcome.")
check(setequal(unique(rich_results$panel), c("Level", "Scaled")), "Panels must follow the panel_by dimension.")
summary_rows <- summarise_results(rich_manifest, rich_results)
check(nrow(summary_rows) == 2L && all(summary_rows$specifications == 6L), "Summaries must be computed per panel.")
plot_directory <- file.path(directory, "rich-plot")
plot_curve(rich_manifest, rich_results, plot_directory)
check(file.exists(file.path(plot_directory, "specification_curve.pdf")), "Rich grids must render.")
duplicate <- rich
duplicate$dimensions[[2L]]$options[[2L]]$filters <- list()
expect_error(validate_menu(duplicate, rich_data), "same model")
conflict <- rich
conflict$dimensions[[1L]]$options[[2L]]$outcome <- "y"
expect_error(validate_menu(conflict, rich_data), "Only one dimension may replace outcome")
bad_exclusion <- rich
bad_exclusion$exclusions <- list(list(match = list(trend = "firm"), reason = ""))
expect_error(validate_menu(bad_exclusion, rich_data), "needs a reason")
excluded_preferred <- rich
excluded_preferred$preferred$window <- "late"
excluded_preferred$preferred$trend <- "firm"
expect_error(validate_menu(excluded_preferred, rich_data), "Preferred specification must be in the grid")

# Time-series means with i.i.d. and Newey-West uncertainty.
set.seed(7)
series <- data.frame(row_id = seq_len(240L), series = rep(c("a", "b"), each = 120L), month = rep(seq_len(120L), 2L))
series$ret <- 0.5 + stats::arima.sim(list(ar = 0.3), n = 240L)
ts_menu <- list(schema_version = 2L, version = "ts-test", analysis_kind = "synthetic_demo",
  estimator = "time_series_mean", title = "Time-series test", estimand = "Mean return",
  row_id = "row_id", unit = "series", time = "month",
  base = list(outcome = "ret", vcov = list(type = "iid"), filters = list()), confidence_level = 0.95,
  dimensions = list(
    list(name = "series", label = "Series", options = list(
      list(id = "a", label = "A", filters = list(list(variable = "series", op = "==", value = "a"))),
      list(id = "b", label = "B", filters = list(list(variable = "series", op = "==", value = "b"))))),
    list(name = "se", label = "Uncertainty", options = list(
      list(id = "iid", label = "i.i.d.", vcov = list(type = "iid")),
      list(id = "nw", label = "Newey-West", vcov = list(type = "newey_west", lags = 4L))))),
  sample_modes = list("available"), preferred = list(series = "a", se = "iid", sample_mode = "available"))
ts_manifest <- freeze_manifest(ts_menu, series, root, file.path(directory, "ts.json"))
ts_results <- run_manifest(ts_manifest, series)
check(validate_results(ts_manifest, ts_results)$passed, "Time-series grid should complete.")
a <- series$ret[series$series == "a"]
iid_row <- ts_results[ts_results$series == "a" & ts_results$se == "iid", ]
check(abs(iid_row$estimate - mean(a)) < 1e-12 && abs(iid_row$std_error - stats::sd(a) / sqrt(120)) < 1e-12,
      "i.i.d. time-series mean must match the textbook formula.")
nw_row <- ts_results[ts_results$series == "a" & ts_results$se == "nw", ]
nw_direct <- sqrt(sandwich::NeweyWest(stats::lm(a ~ 1), lag = 4L, prewhite = FALSE, adjust = FALSE)[1L, 1L])
check(abs(nw_row$std_error - nw_direct) < 1e-12, "Newey-West standard errors must match sandwich.")
pooled <- ts_menu
pooled$dimensions <- pooled$dimensions[2L]
pooled$preferred <- list(se = "iid", sample_mode = "available")
pooled_manifest <- freeze_manifest(pooled, series, root, file.path(directory, "pooled.json"))
pooled_results <- run_manifest(pooled_manifest, series)
check(all(pooled_results$status == "failed") && all(grepl("exactly one series", pooled_results$error)),
      "Time-series means must refuse pooled series.")

# Instrumental variables with fixed effects and the effective first-stage F.
set.seed(7)
ivd <- data.frame(row_id = seq_len(600L), firm = rep(seq_len(60L), each = 10L), year = rep(2001:2010, 60L))
ivd$z1 <- stats::rnorm(600L)
ivd$z2 <- stats::rnorm(600L)
ivd$w <- stats::runif(600L, 0.5, 2)
ivd$c1 <- stats::rnorm(600L)
u <- stats::rnorm(600L)
ivd$x <- 0.8 * ivd$z1 + 0.3 * ivd$z2 + 0.5 * u + stats::rnorm(600L) + ivd$firm / 60
ivd$y <- 1.5 * ivd$x + 0.4 * ivd$c1 + u + ivd$year / 10
iv_menu <- list(schema_version = 2L, version = "iv-test", analysis_kind = "synthetic_demo",
  estimator = "iv_fixed_effects", title = "IV test", estimand = "Effect of x on y",
  row_id = "row_id", unit = "firm", time = "year",
  base = list(outcome = "y", treatment = "x", instruments = list("z1", "z2"), required_controls = list("c1"),
              fixed_effects = list("firm", "year"), vcov = list(type = "cluster", cluster = "firm")),
  confidence_level = 0.95,
  dimensions = list(
    list(name = "instrument_set", label = "Instruments", options = list(
      list(id = "both", label = "Both"),
      list(id = "z1", label = "z1 only", instruments = list("z1"), add_controls = list("z2")))),
    list(name = "weighting", label = "Weights", options = list(
      list(id = "none", label = "Unweighted"),
      list(id = "w", label = "Weighted", weights = "w")))),
  sample_modes = list("available"), preferred = list(instrument_set = "both", weighting = "none", sample_mode = "available"))
iv_manifest <- freeze_manifest(iv_menu, ivd, root, file.path(directory, "iv.json"))
iv_results <- run_manifest(iv_manifest, ivd)
check(validate_results(iv_manifest, iv_results)$passed, "IV grid should complete.")
iv_direct <- fixest::feols(y ~ c1 | firm + year | x ~ z1 + z2, data = ivd, vcov = ~firm, ssc = fixest_ssc(), fixef.tol = 1e-10, notes = FALSE)
iv_pref <- iv_results[iv_results$preferred, ]
check(abs(iv_pref$estimate - unname(stats::coef(iv_direct)["fit_x"])) < 1e-12 &&
        abs(iv_pref$std_error - unname(fixest::se(iv_direct)["fit_x"])) < 1e-12,
      "IV estimate and standard error must match a direct fixest call.")
one <- iv_results[iv_results$instrument_set == "z1" & iv_results$weighting == "none", ]
first <- fixest::feols(x ~ z1 + z2 + c1 | firm + year, data = ivd, vcov = ~firm, ssc = fixest_ssc(), fixef.tol = 1e-10, notes = FALSE)
check(abs(one$first_stage_f - unname(fixest::tstat(first)["z1"])^2) < 1e-8,
      "With one instrument the effective F must equal the squared robust first-stage t statistic.")
check(all(iv_results$first_stage_f > 10), "Strong synthetic instruments must report large effective F.")
weighted <- iv_results[iv_results$instrument_set == "both" & iv_results$weighting == "w", ]
w_direct <- fixest::feols(y ~ c1 | firm + year | x ~ z1 + z2, data = ivd, weights = ~w, vcov = ~firm,
                          ssc = fixest_ssc(), fixef.tol = 1e-10, notes = FALSE)
check(abs(weighted$estimate - unname(stats::coef(w_direct)["fit_x"])) < 1e-12 && weighted$weights == "w",
      "Weighted IV must match a direct weighted fixest call and record its weights.")
check(all(grepl("| x ~ z1", iv_results$formula, fixed = TRUE)), "IV formulas must name the endogenous variable and instruments.")
bad_iv <- iv_menu
bad_iv$base$instruments <- list()
expect_error(validate_menu(bad_iv, ivd), "at least one instrument")
bad_iv <- iv_menu
bad_iv$base$instruments <- list("z1", "c1")
expect_error(validate_menu(bad_iv, ivd), "distinct from")
ols_with_z <- iv_menu
ols_with_z$estimator <- "linear_fixed_effects"
expect_error(validate_menu(ols_with_z, ivd), "only used by iv_fixed_effects")
negative <- ivd
negative$w[3L] <- -1
expect_error(validate_menu(iv_menu, negative), "positive")

# Language-neutral execution plan for Stata.
plan <- execution_plan(ts_manifest, series)
check(nrow(plan) == ts_manifest$expected_rows && identical(plan$spec_id, ts_manifest$grid$spec_id),
      "The plan must contain every frozen specification in order.")
check(any(grepl('series == "a"', plan$stata_if, fixed = TRUE)), "String filters must be quoted for Stata.")
check(all(plan$vcov_type[ts_manifest$grid$se == "nw"] == "newey_west") && all(plan$lags[ts_manifest$grid$se == "nw"] == 4L),
      "Newey-West lags must be exported.")
rich_plan <- execution_plan(rich_manifest, rich_data)
check(any(grepl("(year >= 2004 & !missing(year))", rich_plan$stata_if, fixed = TRUE)),
      "Numeric filters must be exported without quotes.")
check(all(rich_plan$unit_trend[rich_manifest$grid$trend == "firm"] == 1L), "Unit trends must be exported.")
check(identical(stata_filter(list(list(variable = "state", op = "not_in", value = c(15, 49))), rich_data),
                "!inlist(state, 15, 49)"), "Set filters must use inlist.")
check(sample_sha256(c(3, 1, 2, 100000)) == "9e8547d120531efa6d39d02233c788335a8968bf7bc39b3027ea4b1634862484",
      "Sample hashes must match the language-neutral definition used by Python.")

# Exercise the public CLI, including CSV round trips and saved failure reports.
input <- file.path(directory, "generated-input.csv")
utils::write.csv(data, input, row.names = FALSE)
application$reference$source <- "Synthetic CLI integration test"
menu_path <- file.path(directory, "application-menu.json")
jsonlite::write_json(application, menu_path, auto_unbox = TRUE, null = "null", digits = NA)
cli_manifest <- file.path(directory, "cli-manifest.json")
runner <- file.path(root, "paper/run.R")
cli <- function(arguments) suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
  shQuote(c(runner, arguments)), stdout = TRUE, stderr = TRUE))
frozen <- cli(c("freeze", menu_path, input, cli_manifest))
check(is.null(attr(frozen, "status")), paste("CLI freeze failed:", paste(frozen, collapse = "\n")))
cli_output <- file.path(directory, "cli-output")
estimated <- cli(c("estimate", cli_manifest, input, cli_output))
check(is.null(attr(estimated, "status")) && file.exists(file.path(cli_output, "specification_curve.pdf")),
      paste("CLI estimate failed:", paste(estimated, collapse = "\n")))
application$reference$estimate <- 100
jsonlite::write_json(application, menu_path, auto_unbox = TRUE, null = "null", digits = NA)
failure_manifest <- file.path(directory, "failure-manifest.json")
failure_frozen <- cli(c("freeze", menu_path, input, failure_manifest))
check(is.null(attr(failure_frozen, "status")), "Failure-case manifest must freeze successfully.")
failure_output <- file.path(directory, "failure-output")
failed <- cli(c("estimate", failure_manifest, input, failure_output))
check(!is.null(attr(failed, "status")) && file.exists(file.path(failure_output, "validation.json")),
      "CLI baseline failure must be nonzero and save a validation report.")
check(!file.exists(file.path(failure_output, "specification_curve.pdf")),
      "CLI baseline failure must block figure assembly.")
writeLines("tampered", path)
expect_error(read_manifest(path, data, root), "manifest was modified")

unlink(directory, recursive = TRUE)
cat("Passed", checks, "scientific workflow checks.\n")
