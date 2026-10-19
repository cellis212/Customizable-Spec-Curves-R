args <- commandArgs()
script <- sub("^--file=", "", args[grepl("^--file=", args)][1L])
root <- normalizePath(file.path(dirname(script), "../.."))
options(speccurve.root = root)  # GMM menus name moment files relative to the repository root
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

# Generalized method of moments with an application-supplied moment function.
# Deterministic data (no random numbers) so the Python tests use the same values.
tt <- seq_len(200)
gmm_data <- data.frame(row_id = tt, series = "s", time = tt, z1 = sin(0.37 * tt), z2 = cos(1.13 * tt))
gmm_u <- 0.5 * sin(2.71 * tt + 0.5)
gmm_data$x <- 1 + 0.8 * gmm_data$z1 + 0.5 * gmm_data$z2 + 0.5 * gmm_u + 0.3 * cos(0.91 * tt)
gmm_data$y <- 0.5 + 2 * gmm_data$x + gmm_u
gmm_menu <- list(schema_version = 2L, version = "gmm-test", analysis_kind = "synthetic_demo", estimator = "gmm",
  title = "GMM test", estimand = "Slope b in E[z (y - a - b x)] = 0", row_id = "row_id", unit = "series", time = "time",
  base = list(moments = list(name = "linear_iv", r = "paper/tests/gmm_linear_moments.R",
                             python = "python/tests/gmm_linear_moments.py"),
              parameters = list(list(name = "a", lower = -10, upper = 10), list(name = "b", lower = -10, upper = 10)),
              focal_parameter = "b", moment_options = list(instruments = "both"), weighting = "two_step",
              search = list(type = "local"), start = list(a = 0, b = 0), vcov = list(type = "newey_west", lags = 0L)),
  confidence_level = 0.95,
  dimensions = list(
    list(name = "weighting", label = "Weighting", options = list(
      list(id = "two_step", label = "Two-step", weighting = "two_step"),
      list(id = "identity", label = "Identity", weighting = "identity"),
      list(id = "iterated", label = "Iterated", weighting = "iterated"),
      list(id = "cue", label = "CUE", weighting = "cue"))),
    list(name = "search", label = "Search", options = list(
      list(id = "local", label = "Local", search = list(type = "local")),
      list(id = "multi", label = "Multistart", search = list(type = "multistart", starts = 3L)))),
    list(name = "z", label = "Instruments", options = list(
      list(id = "both", label = "z1 and z2"),
      list(id = "z1", label = "z1 only", moment_options = list(instruments = "z1")))),
    list(name = "hac", label = "Long-run covariance", options = list(
      list(id = "nw0", label = "None", vcov = list(type = "newey_west", lags = 0L)),
      list(id = "varhac", label = "VARHAC", vcov = list(type = "varhac", max_lag = 1L, criterion = "aic"))))),
  exclusions = list(list(match = list(weighting = list("cue"), hac = list("varhac")), reason = "CUE with VARHAC is not supported.")),
  sample_modes = list("available"),
  preferred = list(weighting = "two_step", search = "local", z = "both", hac = "nw0", sample_mode = "available"))
gmm_manifest <- freeze_manifest(gmm_menu, gmm_data, root, file.path(directory, "gmm.json"))
check(all(c("paper/tests/gmm_linear_moments.R", "python/tests/gmm_linear_moments.py") %in% names(gmm_manifest$source_hashes)),
      "GMM manifests must pin the application's moment files.")
gmm_results <- run_manifest(gmm_manifest, gmm_data)
check(validate_results(gmm_manifest, gmm_results)$passed && nrow(gmm_results) == 28L, "GMM grid should complete.")
check(all(gmm_row_fields(gmm_menu) %in% names(gmm_results)) && all(gmm_results$convergence == 0),
      "GMM rows must carry their diagnostics.")
X <- cbind(1, gmm_data$x)
Z <- cbind(1, gmm_data$z1, gmm_data$z2)
n <- nrow(gmm_data)
linear_gmm <- function(W) drop(solve(t(X) %*% Z %*% W %*% t(Z) %*% X, t(X) %*% Z %*% W %*% t(Z) %*% gmm_data$y))
theta1 <- linear_gmm(diag(3))
e1 <- drop(gmm_data$y - X %*% theta1)
S1 <- crossprod(e1 * Z) / n
theta2 <- linear_gmm(solve(S1))
e2 <- drop(gmm_data$y - X %*% theta2)
S2 <- crossprod(e2 * Z) / n
G <- -crossprod(Z, X) / n
gbar2 <- colMeans(e2 * Z)
pick <- function(...) { keep <- rep(TRUE, nrow(gmm_results)); a <- list(...)
  for (k in names(a)) keep <- keep & gmm_results[[k]] == a[[k]]; gmm_results[keep, ] }
row_id <- pick(weighting = "identity", search = "local", z = "both", hac = "nw0")
row_2s <- pick(weighting = "two_step", search = "local", z = "both", hac = "nw0")
check(abs(row_id$estimate - theta1[2L]) < 1e-8 && abs(row_id$theta_a - theta1[1L]) < 1e-8 && is.na(row_id$j_stat),
      "Identity-weighted GMM must match the closed form and report no J test.")
bread <- solve(crossprod(G))
check(abs(row_id$std_error - sqrt((bread %*% t(G) %*% S1 %*% G %*% bread / n)[2L, 2L])) < 1e-7,
      "Identity-weighted GMM must use the sandwich covariance.")
check(abs(row_2s$estimate - theta2[2L]) < 1e-8, "Two-step GMM must match the closed form.")
check(abs(row_2s$std_error - sqrt(solve(t(G) %*% solve(S2, G))[2L, 2L] / n)) < 1e-7,
      "Two-step standard errors must use the covariance at the second-step estimate.")
check(abs(row_2s$j_stat - n * drop(t(gbar2) %*% solve(S2, gbar2))) < 1e-7 && row_2s$j_df == 1L &&
        abs(row_2s$j_p_value - stats::pchisq(row_2s$j_stat, 1, lower.tail = FALSE)) < 1e-12,
      "The J statistic must use the covariance at the second-step estimate.")
row_multi <- pick(weighting = "two_step", search = "multi", z = "both", hac = "nw0")
check(abs(row_multi$estimate - theta2[2L]) < 1e-8 && row_multi$n_starts == 5L && row_multi$start_share_best == 1 &&
        row_multi$start_focal_range < 1e-6, "Multistart must try the previous estimate, the start and the Halton points.")
row_it <- pick(weighting = "iterated", search = "local", z = "both", hac = "nw0")
# The CUE objective is multimodal: from (0, 0) a single local search stops at a
# distant mode, while multistart finds the one below the two-step J statistic.
row_cue <- pick(weighting = "cue", search = "multi", z = "both", hac = "nw0")
row_cue_local <- pick(weighting = "cue", search = "local", z = "both", hac = "nw0")
check(row_it$weighting_updates >= 2L && row_cue$j_stat <= row_2s$j_stat + 1e-9 && row_cue_local$j_stat > row_cue$j_stat,
      "Iterated GMM must update the weighting, and multistart CUE must not exceed the two-step J statistic.")
just <- pick(weighting = "two_step", search = "local", z = "z1", hac = "nw0")
check(just$j_df == 0L && is.na(just$j_p_value) && just$j_stat < 1e-12,
      "Exactly identified GMM has no overidentification test.")
check(all(grepl("^linear_iv\\(instruments=", gmm_results$formula)) && all(gmm_results$treatment == "b"),
      "GMM formulas name the moment model, its options and the focal parameter.")
check(identical(unique(gmm_results$vcov[gmm_results$hac == "varhac"]), "VARHAC(1, AIC)"), "VARHAC must be labelled.")
check(isTRUE(all.equal(halton_point(1, 3), c(0.5, 1 / 3, 0.2))) && isTRUE(all.equal(halton_point(5, 2), c(0.625, 7 / 9))),
      "Halton points must follow the radical-inverse definition.")
check(isTRUE(all.equal(gmm_varhac(cbind(gmm_u), 0L, "aic"), crossprod(cbind(gmm_u)) / n)), "VARHAC(0) is the uncentered second moment.")
ar <- matrix(as.numeric(stats::filter(gmm_u, 0.6, method = "recursive")), ncol = 1L)
a_hat <- sum(ar[-1L] * ar[-n]) / sum(ar[-n]^2)
check(abs(gmm_varhac(ar, 1L, "aic")[1L, 1L] - mean((ar[-1L] - a_hat * ar[-n])^2) / (1 - a_hat)^2) < 1e-12,
      "VARHAC(1) for one AR(1) series must equal sigma^2 / (1 - a)^2.")
# Infeasible starts, bounds and unsupported combinations.
broken <- gmm_menu
broken$base$moment_options$broken <- TRUE
broken$dimensions <- broken$dimensions[1L]
broken$exclusions <- NULL
broken$preferred <- list(weighting = "two_step", sample_mode = "available")
broken_manifest <- freeze_manifest(broken, gmm_data, root, file.path(directory, "gmm-broken.json"))
broken_results <- run_manifest(broken_manifest, gmm_data)
check(nrow(broken_results) == 4L && all(broken_results$status == "failed") &&
        all(grepl("objective not finite at the start", broken_results$error)) && all(broken_results$convergence == 3),
      "Rows whose objective is not finite must be kept as failures with their reason and code.")
check(!validate_results(broken_manifest, broken_results)$passed, "Failed rows are fatal unless the menu allows them.")
broken_manifest$menu$allow_failed_rows <- TRUE
expect_error(validate_results(broken_manifest, broken_results), "preferred specification failed")
bounded <- gmm_menu
bounded$base$parameters[[2L]]$upper <- 1.5
bounded$dimensions <- bounded$dimensions[1L]
bounded$exclusions <- NULL
bounded$preferred <- list(weighting = "two_step", sample_mode = "available")
bounded_results <- run_manifest(freeze_manifest(bounded, gmm_data, root, file.path(directory, "gmm-bounded.json")), gmm_data)
check(all(bounded_results$status == "ok") && all(bounded_results$bound_hits == "b") && all(abs(bounded_results$estimate - 1.5) < 1e-12),
      "Estimates at a bound must be reported with the bound hit.")
unexcluded <- gmm_menu
unexcluded$exclusions <- NULL
expect_error(validate_menu(unexcluded, gmm_data), "CUE with VARHAC")
published <- gmm_menu
published$dimensions[[2L]]$options[[1L]] <- list(id = "local", label = "Published", search = list(type = "published"))
published$base$first_step <- list(a = 0.4, b = 2.1)
expect_error(validate_menu(published, gmm_data), "defines no estimator")
wrong_names <- gmm_menu
wrong_names$base$parameters[[1L]]$name <- "intercept"
expect_error(validate_menu(wrong_names, gmm_data), "must match the moment model")
expect_error(execution_plan(gmm_manifest, gmm_data), "gmm runs in R and Python only")

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
