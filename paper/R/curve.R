# A deliberately scoped reference implementation: linear fixed-effects models
# (optionally weighted), linear instrumental-variables models with fixed effects,
# and time-series means, declared pointwise uncertainty conventions, and
# descriptive sensitivity summaries. It provides no joint inference.

abort_if <- function(condition, message) {
  if (isTRUE(condition)) stop(message, call. = FALSE)
}

sha256 <- function(x) digest::digest(x, algo = "sha256", serialize = TRUE)
file_sha256 <- function(path) digest::digest(file = path, algo = "sha256")

# Language-neutral observation identity: SHA-256 of the sorted identifiers as
# text, one per line, in C-locale order. Whole numbers print without exponents.
id_text <- function(ids) if (is.numeric(ids) && all(ids == round(ids))) sprintf("%.0f", ids) else as.character(ids)
sample_sha256 <- function(ids) digest::digest(paste(sort(id_text(ids), method = "radix"), collapse = "\n"),
                                               algo = "sha256", serialize = FALSE)

source_hashes <- function(root) {
  paths <- c("paper/run.R", "renv.lock", file.path("paper/R",
    sort(list.files(file.path(root, "paper/R"), "[.]R$", full.names = FALSE))))
  stats::setNames(vapply(file.path(root, paths), file_sha256, ""), paths)
}

estimators <- c("linear_fixed_effects", "iv_fixed_effects", "time_series_mean")
filter_ops <- c("==", "!=", ">=", "<=", ">", "<", "in", "not_in")
replacement_fields <- c("outcome", "treatment", "instruments", "fixed_effects", "unit_trend", "vcov", "weights")
result_fields <- c("panel", "formula", "estimator", "outcome", "treatment", "control_vars", "fixed_effect_terms",
                   "unit_trend", "vcov", "filters", "confidence_level", "status", "error", "estimate",
                   "std_error", "statistic", "p_value", "ci_lower", "ci_upper", "n_input", "n_eligible",
                   "n_obs", "n_units", "n_periods", "n_clusters", "n_estimator_dropped", "sample_hash",
                   "dropped_terms", "manifest_hash", "input_hash", "facet", "rank", "role",
                   "instruments", "weights", "first_stage_f")
reserved_names <- c("spec_id", "sample_mode", "preferred", "original", result_fields)
simple_name <- "^[A-Za-z][A-Za-z0-9_]*$"

# Menus are parsed without simplification, so arrays of objects stay lists of
# records and absent optional fields stay NULL.
read_menu <- function(path) jsonlite::read_json(path, simplifyVector = FALSE)

menu_dimensions <- function(menu) menu$dimensions

dimension_names <- function(menu) vapply(menu_dimensions(menu), function(d) d$name, "")

find_option <- function(menu, dimension, id) {
  dims <- menu_dimensions(menu)
  d <- dims[[match(dimension, vapply(dims, function(x) x$name, ""))]]
  d$options[[match(id, vapply(d$options, function(o) o$id, ""))]]
}

normalise_filters <- function(filters) {
  lapply(filters, function(f) {
    f$value <- unlist(f$value)
    f
  })
}

normalise_vcov <- function(vcov) {
  if (is.null(vcov)) return(NULL)
  vcov <- as.list(vcov)
  if (!is.null(vcov$lags)) vcov$lags <- as.numeric(vcov$lags)
  vcov
}

vcov_label <- function(vcov) {
  switch(vcov$type,
    cluster = paste0("cluster(", vcov$cluster, ")"),
    iid = "iid", hetero = "heteroskedasticity-robust",
    newey_west = paste0("Newey-West(", vcov$lags, ")"),
    stop("Unknown vcov type.", call. = FALSE))
}

# Resolve one row of the grid into a model: start from the base model and apply
# each chosen option. Replacements may come from one dimension only; filters
# and controls accumulate.
resolve_spec <- function(menu, choice) {
  base <- menu$base
  spec <- list(outcome = base$outcome, treatment = base$treatment, instruments = unlist(base$instruments),
               controls = unlist(base$required_controls), fixed_effects = unlist(base$fixed_effects),
               unit_trend = isTRUE(base$unit_trend), vcov = normalise_vcov(base$vcov), weights = base$weights,
               filters = normalise_filters(base$filters), panel = "All specifications")
  for (d in menu_dimensions(menu)) {
    option <- find_option(menu, d$name, choice[[d$name]])
    for (field in replacement_fields) if (!is.null(option[[field]])) {
      spec[[field]] <- if (field == "vcov") normalise_vcov(option$vcov) else
        if (field %in% c("fixed_effects", "instruments")) unlist(option[[field]]) else option[[field]]
    }
    spec$controls <- unique(c(spec$controls, unlist(option$add_controls)))
    spec$filters <- c(spec$filters, normalise_filters(option$filters))
    if (identical(d$name, menu$panel_by)) spec$panel <- option$label
  }
  spec$filter_signature <- if (length(spec$filters)) paste(vapply(spec$filters, function(f)
    paste(f$variable, f$op, paste(f$value, collapse = ",")), ""), collapse = " & ") else "none"
  spec
}

spec_variables <- function(menu, spec) {
  unique(c(spec$outcome, spec$treatment, spec$instruments, spec$controls, spec$fixed_effects, spec$weights,
           if (identical(spec$vcov$type, "cluster")) spec$vcov$cluster,
           vapply(spec$filters, function(f) f$variable, ""),
           menu$unit, menu$time, menu$row_id))
}

weights_label <- function(spec) if (is.null(spec$weights)) "none" else spec$weights
instruments_label <- function(spec) if (length(spec$instruments)) paste(spec$instruments, collapse = ";") else ""

model_formula <- function(menu, spec) {
  if (menu$estimator == "time_series_mean") return(paste(spec$outcome, "~ 1"))
  iv <- menu$estimator == "iv_fixed_effects"
  rhs <- if (iv) spec$controls else c(spec$treatment, spec$controls)
  formula <- paste(spec$outcome, "~", if (length(rhs)) paste(rhs, collapse = " + ") else "1")
  fe <- spec$fixed_effects
  if (spec$unit_trend) fe[fe == menu$unit] <- paste0(menu$unit, "[", menu$time, "]")
  if (length(fe)) formula <- paste(formula, "|", paste(fe, collapse = " + "))
  if (iv) formula <- paste(formula, "|", spec$treatment, "~", paste(spec$instruments, collapse = " + "))
  formula
}

choice_grid <- function(menu) {
  dims <- menu_dimensions(menu)
  levels <- stats::setNames(lapply(dims, function(d) vapply(d$options, function(o) o$id, "")),
                            vapply(dims, function(d) d$name, ""))
  levels$sample_mode <- unlist(menu$sample_modes)
  grid <- expand.grid(levels, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  matches <- function(rule) {
    keep <- rep(TRUE, nrow(grid))
    for (name in names(rule$match)) keep <- keep & grid[[name]] %in% unlist(rule$match[[name]])
    keep
  }
  excluded <- rep(FALSE, nrow(grid))
  reasons <- rep("", nrow(grid))
  for (rule in menu$exclusions) {
    hit <- matches(rule) & !excluded
    reasons[hit] <- rule$reason
    excluded <- excluded | hit
  }
  list(grid = grid[!excluded, , drop = FALSE], excluded = cbind(grid[excluded, , drop = FALSE],
       reason = reasons[excluded], stringsAsFactors = FALSE))
}

same_choice <- function(grid, choice) {
  keep <- rep(TRUE, nrow(grid))
  for (name in names(choice)) keep <- keep & grid[[name]] == choice[[name]]
  keep
}

validate_vcov <- function(menu, vcov) {
  abort_if(is.null(vcov$type), "Every specification needs an uncertainty convention.")
  allowed <- if (menu$estimator == "time_series_mean") c("iid", "newey_west") else c("cluster", "iid", "hetero")
  abort_if(!vcov$type %in% allowed, paste("Unsupported vcov for", menu$estimator, ":", vcov$type))
  if (vcov$type == "cluster") abort_if(is.null(vcov$cluster) || !nzchar(vcov$cluster), "Cluster variable missing.")
  if (vcov$type == "newey_west") abort_if(!is.numeric(vcov$lags) || vcov$lags < 0 || vcov$lags != round(vcov$lags),
                                          "Newey-West lags must be a nonnegative integer.")
}

validate_menu <- function(menu, data) {
  fields <- c("schema_version", "version", "analysis_kind", "estimator", "title", "estimand", "row_id",
              "unit", "time", "base", "confidence_level", "dimensions", "sample_modes", "preferred")
  abort_if(!all(fields %in% names(menu)), paste("Menu is missing required fields:",
           paste(setdiff(fields, names(menu)), collapse = ", ")))
  abort_if(!identical(as.integer(menu$schema_version), 2L), "Menu schema_version must be 2.")
  abort_if(!menu$analysis_kind %in% c("synthetic_demo", "application"), "Unknown analysis kind.")
  abort_if(!menu$estimator %in% estimators, paste("Supported estimators:", paste(estimators, collapse = ", ")))
  dims <- menu_dimensions(menu)
  abort_if(!length(dims), "Specify at least one choice dimension.")
  names <- vapply(dims, function(d) d$name, "")
  abort_if(anyDuplicated(names) > 0L || any(!grepl(simple_name, names)) || any(names %in% reserved_names),
           "Dimension names must be unique simple identifiers and not reserved.")
  for (d in dims) {
    ids <- vapply(d$options, function(o) o$id, "")
    abort_if(!length(ids) || anyDuplicated(ids) > 0L, paste("Options must be unique and nonempty:", d$name))
    abort_if(any(!nzchar(vapply(d$options, function(o) if (is.null(o$label)) "" else o$label, ""))),
             paste("Every option needs a label:", d$name))
  }
  for (field in replacement_fields) {
    setters <- vapply(dims, function(d) any(vapply(d$options, function(o) !is.null(o[[field]]), logical(1))), logical(1))
    abort_if(sum(setters) > 1L, paste("Only one dimension may replace", field))
  }
  if (!is.null(menu$panel_by)) abort_if(!menu$panel_by %in% names, "panel_by must name a dimension.")
  abort_if(any(!menu$sample_modes %in% c("available", "common")) || !length(menu$sample_modes) ||
             anyDuplicated(menu$sample_modes) > 0L, "Sample modes must be unique available/common choices.")
  if (menu$estimator == "time_series_mean")
    abort_if(!identical(unlist(menu$sample_modes), "available"), "Time-series means use available samples only.")
  abort_if(!is.numeric(menu$confidence_level) || length(menu$confidence_level) != 1L ||
             !is.finite(menu$confidence_level) || menu$confidence_level <= 0 || menu$confidence_level >= 1,
           "Confidence level must be between zero and one.")
  abort_if(anyNA(data[[menu$row_id]]) || anyDuplicated(data[[menu$row_id]]) > 0L,
           "Observation identifiers must be unique and nonmissing.")
  for (rule in menu$exclusions) {
    abort_if(is.null(rule$reason) || !nzchar(rule$reason), "Every exclusion needs a reason.")
    abort_if(!all(names(rule$match) %in% c(names, "sample_mode")), "Exclusions must reference dimensions.")
  }
  built <- choice_grid(menu)
  abort_if(!nrow(built$grid), "The menu excludes every specification.")
  preferred <- menu$preferred
  abort_if(!setequal(names(preferred), c(names, "sample_mode")),
           "Preferred specification must choose every dimension and the sample mode.")
  abort_if(sum(same_choice(built$grid, preferred)) != 1L, "Preferred specification must be in the grid.")
  for (choice in menu$original_specifications) {
    abort_if(!setequal(names(choice), c(names, "sample_mode")) || sum(same_choice(built$grid, choice)) != 1L,
             "Every original specification must identify one grid row.")
  }
  signatures <- character(nrow(built$grid))
  for (i in seq_len(nrow(built$grid))) {
    spec <- resolve_spec(menu, built$grid[i, , drop = FALSE])
    validate_vcov(menu, spec$vcov)
    regression <- menu$estimator != "time_series_mean"
    abort_if(is.null(spec$outcome) || (regression && is.null(spec$treatment)),
             "Every specification needs an outcome and, for regressions, a focal variable.")
    abort_if(spec$outcome %in% c(spec$controls, spec$treatment, spec$instruments), "The outcome cannot also be a regressor.")
    if (menu$estimator == "iv_fixed_effects") {
      abort_if(!length(spec$instruments), "Instrumental-variables specifications need at least one instrument.")
      abort_if(any(spec$instruments %in% c(spec$treatment, spec$controls)) || anyDuplicated(spec$instruments) > 0L,
               "Instruments must be distinct from the endogenous variable and the controls.")
    } else abort_if(length(spec$instruments) > 0L, "Instruments are only used by iv_fixed_effects.")
    abort_if(!is.null(spec$weights) && !regression, "Weights are only used by regression estimators.")
    if (spec$unit_trend) abort_if(!menu$unit %in% spec$fixed_effects, "Unit trends require unit fixed effects.")
    vars <- spec_variables(menu, spec)
    abort_if(any(!grepl(simple_name, vars)), "Use simple variable names in the menu.")
    abort_if(!all(vars %in% names(data)), paste("Menu references variables absent from input data:",
             paste(setdiff(vars, names(data)), collapse = ", ")))
    abort_if(!is.numeric(data[[spec$outcome]]) || (!is.null(spec$treatment) && !is.numeric(data[[spec$treatment]])) ||
               any(!vapply(data[spec$instruments], is.numeric, logical(1))),
             "Outcome, focal, and instrument variables must be numeric.")
    if (!is.null(spec$weights)) abort_if(!is.numeric(data[[spec$weights]]) || any(data[[spec$weights]] <= 0, na.rm = TRUE),
                                         "Weights must be positive numbers.")
    for (f in spec$filters) abort_if(!f$op %in% filter_ops || is.null(f$value), "Unsupported filter.")
    signatures[i] <- paste(model_formula(menu, spec), vcov_label(spec$vcov), weights_label(spec), spec$filter_signature,
                           built$grid$sample_mode[i], sep = " | ")
  }
  abort_if(anyDuplicated(signatures) > 0L && !isTRUE(menu$allow_duplicate_models),
           "Two grid rows define the same model and sample; remove the redundancy or declare it.")
  if (menu$analysis_kind == "application") {
    ref <- menu$reference
    abort_if(is.null(ref) || !is.numeric(ref$estimate) || length(ref$estimate) != 1L ||
               !is.finite(ref$estimate) || !is.numeric(ref$tolerance) ||
               length(ref$tolerance) != 1L || !is.finite(ref$tolerance) || ref$tolerance < 0 ||
               is.null(ref$source) || !nzchar(ref$source),
             "Applications require a published baseline estimate, tolerance, and source.")
  }
  invisible(built)
}

freeze_manifest <- function(menu, data, root, path) {
  built <- validate_menu(menu, data)
  abort_if(file.exists(path) || file.exists(paste0(path, ".sha256")),
           "Manifest already exists; create a new version rather than overwriting it.")
  grid <- built$grid
  rownames(grid) <- NULL
  grid$spec_id <- sprintf("S%04d", seq_len(nrow(grid)))
  grid$preferred <- same_choice(grid, menu$preferred)
  grid$original <- grid$preferred
  for (choice in menu$original_specifications) grid$original <- grid$original | same_choice(grid, choice)
  manifest <- list(schema_version = 2L, menu = menu,
                   created_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                   expected_rows = nrow(grid), input_hash = sha256(data),
                   source_hashes = as.list(source_hashes(root)), grid = grid,
                   excluded = built$excluded)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(manifest, path, pretty = TRUE, auto_unbox = TRUE, null = "null", digits = NA)
  writeLines(file_sha256(path), paste0(path, ".sha256"))
  read_manifest(path, data, root)
}

read_manifest <- function(path, data, root) {
  abort_if(!file.exists(path) || !file.exists(paste0(path, ".sha256")), "Frozen manifest is missing.")
  hash <- file_sha256(path)
  abort_if(!identical(hash, readLines(paste0(path, ".sha256"), warn = FALSE)), "Frozen manifest was modified.")
  manifest <- jsonlite::read_json(path, simplifyVector = TRUE)
  manifest$menu <- jsonlite::read_json(path, simplifyVector = FALSE)$menu
  validate_menu(manifest$menu, data)
  abort_if(!identical(manifest$input_hash, sha256(data)), "Input data differ from the frozen manifest.")
  current <- source_hashes(root)
  expected <- unlist(manifest$source_hashes)
  abort_if(!identical(current, expected), "Analysis source differs from the frozen manifest.")
  grid <- manifest$grid
  abort_if(nrow(grid) != manifest$expected_rows || anyDuplicated(grid$spec_id) > 0L ||
             sum(grid$preferred) != 1L, "Manifest grid is incomplete or has invalid identifiers.")
  manifest$manifest_hash <- hash
  manifest
}

apply_filters <- function(data, filters) {
  keep <- rep(TRUE, nrow(data))
  for (f in filters) {
    x <- data[[f$variable]]
    hit <- switch(f$op,
      "==" = x == f$value, "!=" = x != f$value, ">=" = x >= f$value, "<=" = x <= f$value,
      ">" = x > f$value, "<" = x < f$value, "in" = x %in% f$value, "not_in" = !(x %in% f$value))
    keep <- keep & !is.na(hit) & hit
  }
  keep
}

newey_west_se <- function(y, lags) {
  n <- length(y)
  u <- y - mean(y)
  s <- sum(u^2) / n
  if (lags > 0L) for (l in seq_len(min(lags, n - 1L)))
    s <- s + 2 * (1 - l / (lags + 1)) * sum(u[(l + 1):n] * u[seq_len(n - l)]) / n
  sqrt(s / n)
}

estimate_time_series_mean <- function(menu, spec, frame) {
  abort_if(length(unique(frame[[menu$unit]])) != 1L, "A time-series mean needs exactly one series.")
  abort_if(anyDuplicated(frame[[menu$time]]) > 0L, "Duplicate periods in a time series.")
  frame <- frame[order(frame[[menu$time]]), , drop = FALSE]
  y <- frame[[spec$outcome]]
  n <- length(y)
  abort_if(n < 3L, "Fewer than three periods.")
  se <- if (spec$vcov$type == "iid") stats::sd(y) / sqrt(n) else newey_west_se(y, as.integer(spec$vcov$lags))
  estimate <- mean(y)
  statistic <- estimate / se
  critical <- stats::qt(1 - (1 - menu$confidence_level) / 2, df = n - 1L)
  list(estimate = estimate, std_error = se, statistic = statistic,
       p_value = 2 * stats::pt(-abs(statistic), df = n - 1L),
       ci_lower = estimate - critical * se, ci_upper = estimate + critical * se,
       n_obs = n, used = frame, dropped_terms = "", n_clusters = NA_integer_)
}

fixest_ssc <- function() fixest::ssc(K.adj = TRUE, K.fixef = "nonnested", G.adj = TRUE, t.df = "min")

# Fixed effects are removed by iterative demeaning; a tight convergence tolerance
# (fixest's default is 1e-6) keeps results independent of the algorithm, so R and
# Python agree to about 1e-10 even with several unbalanced fixed effects.

fit_feols <- function(formula_text, frame, spec, ...) {
  vcov <- switch(spec$vcov$type, cluster = stats::as.formula(paste("~", spec$vcov$cluster)),
                 iid = "iid", hetero = "hetero")
  weights <- if (is.null(spec$weights)) NULL else stats::as.formula(paste("~", spec$weights))
  fixest::feols(stats::as.formula(formula_text), data = frame, vcov = vcov, weights = weights, ssc = fixest_ssc(),
                fixef.tol = 1e-10, notes = FALSE, warn = FALSE, nthreads = 1L, vcov_fix = FALSE, ...)
}

# Montiel Olea-Pflueger effective first-stage F for one endogenous regressor:
# pi' (Z'WZ) pi / trace(V_pi Z'WZ), with Z the instruments after partialling out
# the controls and fixed effects, and V_pi the first-stage covariance under the
# specification's own uncertainty convention (equal to the robust Wald F when
# there is one instrument).
effective_first_stage_f <- function(spec, used) {
  rhs <- c(spec$instruments, spec$controls)
  fe <- if (length(spec$fixed_effects)) paste("|", paste(spec$fixed_effects, collapse = " + ")) else ""
  first <- fit_feols(paste(spec$treatment, "~", paste(rhs, collapse = " + "), fe), used, spec)
  abort_if(stats::nobs(first) != nrow(used) || !all(spec$instruments %in% names(stats::coef(first))),
           "First stage is not identified on the estimation sample.")
  pi <- stats::coef(first)[spec$instruments]
  v <- stats::vcov(first)[spec$instruments, spec$instruments, drop = FALSE]
  partial <- vapply(spec$instruments, function(z) {
    rhs_z <- if (length(spec$controls)) paste(spec$controls, collapse = " + ") else "1"
    stats::residuals(fit_feols(paste(z, "~", rhs_z, fe), used, utils::modifyList(spec, list(vcov = list(type = "iid"))),
                               fixef.rm = "none"))
  }, numeric(nrow(used)))
  w <- if (is.null(spec$weights)) rep(1, nrow(used)) else used[[spec$weights]]
  zz <- crossprod(partial * sqrt(w))
  drop(t(pi) %*% zz %*% pi) / sum(diag(v %*% zz))
}

estimate_fixed_effects <- function(menu, spec, frame, formula_text) {
  model <- fit_feols(formula_text, frame, spec)
  iv <- menu$estimator == "iv_fixed_effects"
  focal <- if (iv) paste0("fit_", spec$treatment) else spec$treatment
  table <- fixest::coeftable(model)
  abort_if(!focal %in% rownames(table), "Focal coefficient is unidentified or absorbed.")
  abort_if(any(unlist(menu$base$required_controls) %in% model$collin.var),
           "A required control is unidentified or absorbed.")
  interval <- stats::confint(model, parm = focal, level = menu$confidence_level)
  used <- frame[fixest::obs(model), , drop = FALSE]
  list(estimate = unname(table[focal, 1L]), std_error = unname(table[focal, 2L]),
       statistic = unname(table[focal, 3L]), p_value = unname(table[focal, 4L]),
       ci_lower = unname(interval[1L, 1L]), ci_upper = unname(interval[1L, 2L]),
       n_obs = stats::nobs(model), used = used, dropped_terms = paste(model$collin.var, collapse = ";"),
       n_clusters = if (spec$vcov$type == "cluster") length(unique(used[[spec$vcov$cluster]])) else NA_integer_,
       first_stage_f = if (iv) effective_first_stage_f(spec, used) else NA_real_)
}

run_manifest <- function(manifest, data) {
  menu <- manifest$menu
  validate_menu(menu, data)
  abort_if(!identical(manifest$input_hash, sha256(data)), "Input data changed after freezing.")
  grid <- manifest$grid
  dims <- dimension_names(menu)
  specs <- lapply(seq_len(nrow(grid)), function(i) resolve_spec(menu, grid[i, , drop = FALSE]))
  common_vars <- unique(unlist(lapply(specs, function(spec) spec_variables(menu, spec))))
  common <- stats::complete.cases(data[, common_vars, drop = FALSE])
  rows <- lapply(seq_len(nrow(grid)), function(i) {
    spec <- specs[[i]]
    vars <- spec_variables(menu, spec)
    complete <- if (grid$sample_mode[i] == "common") common else stats::complete.cases(data[, vars, drop = FALSE])
    frame <- data[complete & apply_filters(data, spec$filters), , drop = FALSE]
    formula_text <- model_formula(menu, spec)
    row <- cbind(grid[i, , drop = FALSE], data.frame(panel = spec$panel, formula = formula_text,
      estimator = menu$estimator, outcome = spec$outcome,
      treatment = if (is.null(spec$treatment)) "(intercept)" else spec$treatment,
      control_vars = paste(spec$controls, collapse = ";"), fixed_effect_terms = paste(spec$fixed_effects, collapse = ";"),
      unit_trend = spec$unit_trend, vcov = vcov_label(spec$vcov), filters = spec$filter_signature,
      instruments = instruments_label(spec), weights = weights_label(spec), first_stage_f = NA_real_,
      confidence_level = menu$confidence_level, status = "failed", error = "",
      estimate = NA_real_, std_error = NA_real_, statistic = NA_real_, p_value = NA_real_,
      ci_lower = NA_real_, ci_upper = NA_real_, n_input = nrow(data), n_eligible = nrow(frame),
      n_obs = NA_integer_, n_units = NA_integer_, n_periods = NA_integer_, n_clusters = NA_integer_,
      n_estimator_dropped = NA_integer_, sample_hash = NA_character_, dropped_terms = "",
      manifest_hash = manifest$manifest_hash, input_hash = manifest$input_hash,
      stringsAsFactors = FALSE))
    tryCatch({
      abort_if(!nrow(frame), "No complete observations.")
      fit <- if (menu$estimator == "time_series_mean") estimate_time_series_mean(menu, spec, frame) else
        estimate_fixed_effects(menu, spec, frame, formula_text)
      for (field in c("estimate", "std_error", "statistic", "p_value", "ci_lower", "ci_upper", "n_obs",
                      "dropped_terms", "n_clusters")) row[[field]] <- fit[[field]]
      if (!is.null(fit$first_stage_f)) row$first_stage_f <- fit$first_stage_f
      row$n_units <- length(unique(fit$used[[menu$unit]]))
      row$n_periods <- length(unique(fit$used[[menu$time]]))
      row$n_estimator_dropped <- nrow(frame) - row$n_obs
      row$sample_hash <- sample_sha256(fit$used[[menu$row_id]])
      abort_if(any(!is.finite(unlist(row[c("estimate", "std_error", "p_value", "ci_lower", "ci_upper")]))),
               "Uncertainty is not finite.")
      abort_if(spec$vcov$type == "cluster" && row$n_clusters < 2L, "Fewer than two clusters remain.")
      abort_if(menu$estimator == "iv_fixed_effects" && !is.finite(row$first_stage_f), "First-stage F is not finite.")
      row$status <- "ok"
    }, error = function(e) {
      row$error <<- conditionMessage(e)
    })
    row
  })
  do.call(rbind, rows)
}

validate_results <- function(manifest, results) {
  grid <- manifest$grid
  menu <- manifest$menu
  abort_if(anyDuplicated(results$spec_id) > 0L || nrow(results) != manifest$expected_rows ||
             !setequal(results$spec_id, grid$spec_id), "Results must contain exactly one row for every manifest ID.")
  results <- results[match(grid$spec_id, results$spec_id), , drop = FALSE]
  for (field in names(grid)) abort_if(!identical(as.vector(results[[field]]), as.vector(grid[[field]])),
                                      paste("Specification labels changed:", field))
  abort_if(anyNA(results[, c("manifest_hash", "input_hash", "estimator", "outcome", "treatment", "vcov",
                             "confidence_level", "status")]),
           "Result provenance and conventions must be nonmissing.")
  abort_if(any(results$manifest_hash != manifest$manifest_hash) || any(results$input_hash != manifest$input_hash),
           "Result provenance does not match the frozen manifest.")
  for (i in seq_len(nrow(grid))) {
    spec <- resolve_spec(menu, grid[i, , drop = FALSE])
    abort_if(results$outcome[i] != spec$outcome || results$vcov[i] != vcov_label(spec$vcov) ||
               results$estimator[i] != menu$estimator || results$confidence_level[i] != menu$confidence_level ||
               results$formula[i] != model_formula(menu, spec) || results$filters[i] != spec$filter_signature ||
               (!is.null(results$weights) && !identical(as.character(results$weights[i]), weights_label(spec))) ||
               (!is.null(results$instruments) && !identical(ifelse(is.na(results$instruments[i]), "", as.character(results$instruments[i])),
                                                            instruments_label(spec))),
             paste("Result convention changed for", grid$spec_id[i]))
  }
  abort_if(any(!results$status %in% c("ok", "failed")), "Every result needs an explicit status.")
  failed <- results$status == "failed"
  abort_if(any(failed & !nzchar(results$error)), "Failed specifications need an explicit reason.")
  for (field in c("estimate", "std_error", "p_value", "ci_lower", "ci_upper", "n_obs",
                  "n_eligible", "n_units", "n_periods", "n_estimator_dropped")) {
    abort_if(any(!is.finite(results[[field]][!failed])), paste("Successful rows have invalid", field))
  }
  ok <- results[!failed, , drop = FALSE]
  abort_if(anyNA(ok$sample_hash) || any(!grepl("^[0-9a-f]{64}$", ok$sample_hash)), "Successful rows need observation hashes.")
  abort_if(any(ok$std_error < 0 | ok$p_value < 0 | ok$p_value > 1 | ok$ci_lower > ok$estimate |
                 ok$ci_upper < ok$estimate | ok$n_obs <= 0), "Invalid uncertainty or sample size.")
  if (menu$estimator == "iv_fixed_effects")
    abort_if(any(!is.finite(ok$first_stage_f) | ok$first_stage_f < 0), "IV rows need a first-stage F statistic.")
  clustered <- grepl("^cluster", ok$vcov)
  abort_if(any(!is.finite(ok$n_clusters[clustered]) | ok$n_clusters[clustered] < 2), "Clustered rows need clusters.")
  common <- ok[ok$sample_mode == "common", , drop = FALSE]
  for (signature in unique(common$filters))
    abort_if(length(unique(common$sample_hash[common$filters == signature])) > 1L,
             "Common-sample specifications used different observations.")
  preferred <- results[results$preferred, , drop = FALSE]
  baseline <- "not_applicable_synthetic_demo"
  reference <- menu$reference
  if (!is.null(reference)) {
    abort_if(preferred$status != "ok" || abs(preferred$estimate - reference$estimate) > reference$tolerance,
             "Preferred specification does not reproduce the documented published baseline.")
    baseline <- "passed"
  }
  list(passed = !any(failed), expected_rows = manifest$expected_rows, completed_rows = sum(!failed),
       failed_ids = as.list(results$spec_id[failed]), baseline_reproduction = baseline,
       manifest_hash = manifest$manifest_hash, joint_inference = "not_implemented")
}

summarise_results <- function(manifest, results) {
  alpha <- 1 - manifest$menu$confidence_level
  ok <- results[results$status == "ok", , drop = FALSE]
  groups <- split(ok, list(ok$panel, ok$sample_mode), drop = TRUE)
  do.call(rbind, lapply(groups, function(rows) {
    baseline <- rows$estimate[rows$preferred]
    data.frame(panel = rows$panel[1L], sample_mode = rows$sample_mode[1L], specifications = nrow(rows),
      median_estimate = stats::median(rows$estimate), minimum_estimate = min(rows$estimate),
      maximum_estimate = max(rows$estimate), fraction_positive = mean(rows$estimate > 0),
      fraction_significant_positive = mean(rows$p_value < alpha & rows$estimate > 0),
      fraction_significant_negative = mean(rows$p_value < alpha & rows$estimate < 0),
      preferred_estimate = if (length(baseline)) baseline else NA_real_,
      preferred_percentile = if (length(baseline)) mean(rows$estimate <= baseline) else NA_real_,
      interpretation = "Descriptive within the specified set; not a hypothesis probability",
      row.names = NULL, stringsAsFactors = FALSE)
  }))
}

# A language-neutral execution plan for other runtimes (e.g. Stata): one row per
# frozen specification with every resolved model element spelled out.
stata_filter <- function(filters, data) {
  if (!length(filters)) return("1")
  quote <- function(variable, value) if (is.character(data[[variable]])) paste0('"', value, '"') else
    format(value, scientific = FALSE, trim = TRUE)
  parts <- vapply(filters, function(f) {
    values <- vapply(f$value, function(v) quote(f$variable, v), "")
    switch(f$op,
      "in" = paste0("inlist(", f$variable, ", ", paste(values, collapse = ", "), ")"),
      "not_in" = paste0("!inlist(", f$variable, ", ", paste(values, collapse = ", "), ")"),
      paste0("(", f$variable, " ", f$op, " ", values[1L], " & !missing(", f$variable, "))"))
  }, "")
  paste(parts, collapse = " & ")
}

execution_plan <- function(manifest, data) {
  menu <- manifest$menu
  grid <- manifest$grid
  specs <- lapply(seq_len(nrow(grid)), function(i) resolve_spec(menu, grid[i, , drop = FALSE]))
  common_vars <- unique(unlist(lapply(specs, function(spec) spec_variables(menu, spec))))
  do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
    spec <- specs[[i]]
    data.frame(spec_id = grid$spec_id[i], sample_mode = grid$sample_mode[i], panel = spec$panel,
      estimator = menu$estimator, outcome = spec$outcome,
      treatment = if (is.null(spec$treatment)) "" else spec$treatment,
      instruments = paste(spec$instruments, collapse = " "), weights = if (is.null(spec$weights)) "" else spec$weights,
      controls = paste(spec$controls, collapse = " "), fixed_effects = paste(spec$fixed_effects, collapse = " "),
      unit_trend = as.integer(spec$unit_trend), unit = menu$unit, time = menu$time, row_id = menu$row_id,
      vcov_type = spec$vcov$type, cluster = if (is.null(spec$vcov$cluster)) "" else spec$vcov$cluster,
      lags = if (is.null(spec$vcov$lags)) NA_integer_ else as.integer(spec$vcov$lags),
      confidence_level = menu$confidence_level, filters = spec$filter_signature,
      stata_if = stata_filter(spec$filters, data),
      sample_vars = paste(if (grid$sample_mode[i] == "common") common_vars else spec_variables(menu, spec), collapse = " "),
      formula = model_formula(menu, spec), vcov = vcov_label(spec$vcov), stringsAsFactors = FALSE)
  }))
}
