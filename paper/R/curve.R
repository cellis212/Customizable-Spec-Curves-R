# A deliberately scoped reference implementation: linear fixed-effects models,
# one-way cluster-robust pointwise uncertainty, and descriptive sensitivity.

abort_if <- function(condition, message) {
  if (isTRUE(condition)) stop(message, call. = FALSE)
}

sha256 <- function(x) digest::digest(x, algo = "sha256", serialize = TRUE)
file_sha256 <- function(path) digest::digest(file = path, algo = "sha256")

source_hashes <- function(root) {
  paths <- c("paper/run.R", "renv.lock", file.path("paper/R",
    sort(list.files(file.path(root, "paper/R"), "[.]R$", full.names = FALSE))))
  stats::setNames(vapply(file.path(root, paths), file_sha256, ""), paths)
}

validate_menu <- function(menu, data) {
  fields <- c("version", "analysis_kind", "estimator", "estimand", "outcome", "treatment", "row_id",
              "unit", "time", "required_controls", "fixed_effects", "cluster",
              "confidence_level", "control_blocks", "sample_modes", "preferred")
  abort_if(!all(fields %in% names(menu)), "Menu is missing required fields.")
  abort_if(!menu$analysis_kind %in% c("synthetic_demo", "application"), "Unknown analysis kind.")
  abort_if(!identical(menu$estimator, "linear_fixed_effects"), "This runner supports linear fixed-effects models only.")
  vars <- unique(c(menu$outcome, menu$treatment, menu$row_id, menu$unit, menu$time,
                   menu$cluster, menu$required_controls, menu$fixed_effects,
                   unlist(menu$control_blocks, use.names = FALSE)))
  abort_if(!all(vars %in% names(data)), "Menu references variables absent from input data.")
  abort_if(any(!grepl("^[A-Za-z][A-Za-z0-9_]*$", vars)), "Use simple variable names in the menu.")
  abort_if(anyNA(data[[menu$row_id]]) || anyDuplicated(data[[menu$row_id]]) > 0L,
           "Observation identifiers must be unique and nonmissing.")
  abort_if(!is.list(menu$control_blocks) || !length(menu$control_blocks) ||
             is.null(names(menu$control_blocks)) || anyDuplicated(names(menu$control_blocks)) > 0L,
           "Control blocks must be a named, nonempty list.")
  block_keys <- vapply(menu$control_blocks, function(x)
    paste(sort(unique(c(menu$required_controls, unlist(x)))), collapse = "+"), "")
  abort_if(anyDuplicated(block_keys) > 0L, "Redundant control blocks are not allowed.")
  abort_if(any(!menu$sample_modes %in% c("available", "common")) ||
             !length(menu$sample_modes) || anyDuplicated(menu$sample_modes) > 0L,
           "Sample modes must be unique available/common choices.")
  abort_if(!is.numeric(menu$confidence_level) || length(menu$confidence_level) != 1L ||
             !is.finite(menu$confidence_level) || menu$confidence_level <= 0 || menu$confidence_level >= 1,
           "Confidence level must be between zero and one.")
  abort_if(!menu$preferred$control_block %in% names(menu$control_blocks) ||
             !menu$preferred$sample_mode %in% menu$sample_modes,
           "Preferred specification must be in the grid.")
  abort_if(!is.numeric(data[[menu$outcome]]) || !is.numeric(data[[menu$treatment]]),
           "This reference implementation requires numeric outcome and focal variables.")
  abort_if(menu$outcome %in% c(menu$required_controls, unlist(menu$control_blocks)),
           "The outcome cannot also be a control.")
  if (menu$analysis_kind == "application") {
    ref <- menu$reference
    abort_if(is.null(ref) || !is.numeric(ref$estimate) || length(ref$estimate) != 1L ||
               !is.finite(ref$estimate) || !is.numeric(ref$tolerance) ||
               length(ref$tolerance) != 1L || !is.finite(ref$tolerance) || ref$tolerance < 0 ||
               is.null(ref$source) || !nzchar(ref$source),
             "Applications require a published baseline estimate, tolerance, and source.")
  }
  invisible(vars)
}

freeze_manifest <- function(menu, data, root, path) {
  validate_menu(menu, data)
  abort_if(file.exists(path) || file.exists(paste0(path, ".sha256")),
           "Manifest already exists; create a new version rather than overwriting it.")
  grid <- expand.grid(control_block = names(menu$control_blocks),
                      sample_mode = menu$sample_modes, stringsAsFactors = FALSE)
  grid$spec_id <- sprintf("S%03d", seq_len(nrow(grid)))
  grid$preferred <- grid$control_block == menu$preferred$control_block &
    grid$sample_mode == menu$preferred$sample_mode
  manifest <- list(schema_version = 1L, menu = menu,
                   created_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                   expected_rows = nrow(grid), input_hash = sha256(data),
                   source_hashes = as.list(source_hashes(root)), grid = grid)
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

spec_variables <- function(menu, block) {
  unique(c(menu$outcome, menu$treatment, menu$required_controls,
           unlist(menu$control_blocks[[block]], use.names = FALSE),
           menu$fixed_effects, menu$cluster, menu$unit, menu$time, menu$row_id))
}

run_manifest <- function(manifest, data) {
  menu <- manifest$menu
  validate_menu(menu, data)
  abort_if(!identical(manifest$input_hash, sha256(data)), "Input data changed after freezing.")
  common_vars <- unique(unlist(lapply(names(menu$control_blocks), function(block) spec_variables(menu, block))))
  common <- stats::complete.cases(data[, common_vars, drop = FALSE])
  rows <- lapply(seq_len(nrow(manifest$grid)), function(i) {
    spec <- manifest$grid[i, , drop = FALSE]
    controls <- unique(c(menu$required_controls, unlist(menu$control_blocks[[spec$control_block]])))
    vars <- spec_variables(menu, spec$control_block)
    selected <- if (spec$sample_mode == "common") common else stats::complete.cases(data[, vars, drop = FALSE])
    frame <- data[selected, , drop = FALSE]
    rhs <- paste(c(menu$treatment, controls), collapse = " + ")
    formula_text <- paste(menu$outcome, "~", rhs)
    if (length(menu$fixed_effects)) formula_text <- paste(formula_text, "|", paste(menu$fixed_effects, collapse = " + "))
    row <- cbind(spec, data.frame(formula = formula_text, estimator = menu$estimator, outcome = menu$outcome,
      treatment = menu$treatment, clustering = menu$cluster, confidence_level = menu$confidence_level,
      status = "failed", error = "", estimate = NA_real_, std_error = NA_real_, p_value = NA_real_,
      ci_lower = NA_real_, ci_upper = NA_real_, n_input = nrow(data), n_eligible = nrow(frame),
      n_obs = NA_integer_, n_units = NA_integer_, n_periods = NA_integer_, n_clusters = NA_integer_,
      n_estimator_dropped = NA_integer_, sample_hash = NA_character_, dropped_terms = "",
      manifest_hash = manifest$manifest_hash, input_hash = manifest$input_hash,
      stringsAsFactors = FALSE))
    tryCatch({
      abort_if(!nrow(frame), "No complete observations.")
      model <- fixest::feols(stats::as.formula(formula_text), data = frame,
        vcov = stats::as.formula(paste("~", menu$cluster)),
        ssc = fixest::ssc(K.adj = TRUE, K.fixef = "nonnested", G.adj = TRUE, t.df = "min"),
        notes = FALSE, warn = FALSE, nthreads = 1L)
      table <- fixest::coeftable(model)
      abort_if(!menu$treatment %in% rownames(table), "Focal coefficient is unidentified or absorbed.")
      row$dropped_terms <- paste(model$collin.var, collapse = ";")
      abort_if(any(menu$required_controls %in% model$collin.var), "A required control is unidentified or absorbed.")
      interval <- stats::confint(model, parm = menu$treatment, level = menu$confidence_level)
      used <- frame[fixest::obs(model), , drop = FALSE]
      row$estimate <- unname(table[menu$treatment, 1L])
      row$std_error <- unname(table[menu$treatment, 2L])
      row$p_value <- unname(table[menu$treatment, 4L])
      row$ci_lower <- unname(interval[1L, 1L])
      row$ci_upper <- unname(interval[1L, 2L])
      row$n_obs <- stats::nobs(model)
      row$n_units <- length(unique(used[[menu$unit]]))
      row$n_periods <- length(unique(used[[menu$time]]))
      row$n_clusters <- length(unique(used[[menu$cluster]]))
      row$n_estimator_dropped <- nrow(frame) - row$n_obs
      row$sample_hash <- sha256(sort(as.character(used[[menu$row_id]])))
      abort_if(row$n_clusters < 2L || any(!is.finite(unlist(row[c("estimate", "std_error", "p_value", "ci_lower", "ci_upper")]))),
               "Uncertainty is not finite or fewer than two clusters remain.")
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
  abort_if(anyDuplicated(results$spec_id) > 0L || nrow(results) != manifest$expected_rows ||
             !setequal(results$spec_id, grid$spec_id), "Results must contain exactly one row for every manifest ID.")
  results <- results[match(grid$spec_id, results$spec_id), , drop = FALSE]
  for (field in names(grid)) abort_if(!identical(results[[field]], grid[[field]]), paste("Specification labels changed:", field))
  abort_if(anyNA(results[, c("manifest_hash", "input_hash", "estimator", "outcome", "treatment", "clustering", "confidence_level", "status")]),
           "Result provenance and conventions must be nonmissing.")
  abort_if(any(results$manifest_hash != manifest$manifest_hash) || any(results$input_hash != manifest$input_hash),
           "Result provenance does not match the frozen manifest.")
  for (field in c("estimator", "outcome", "treatment", "clustering", "confidence_level")) {
    expected <- if (field == "clustering") manifest$menu$cluster else manifest$menu[[field]]
    abort_if(any(results[[field]] != expected), paste("Result convention changed:", field))
  }
  abort_if(any(!results$status %in% c("ok", "failed")), "Every result needs an explicit status.")
  failed <- results$status == "failed"
  abort_if(any(failed & !nzchar(results$error)), "Failed specifications need an explicit reason.")
  for (field in c("estimate", "std_error", "p_value", "ci_lower", "ci_upper", "n_obs", "n_clusters",
                  "n_eligible", "n_units", "n_periods", "n_estimator_dropped")) {
    abort_if(any(!is.finite(results[[field]][!failed])), paste("Successful rows have invalid", field))
  }
  ok <- results[!failed, , drop = FALSE]
  abort_if(anyNA(ok$sample_hash) || any(!grepl("^[0-9a-f]{64}$", ok$sample_hash)), "Successful rows need observation hashes.")
  abort_if(any(ok$std_error < 0 | ok$p_value < 0 | ok$p_value > 1 | ok$ci_lower > ok$estimate |
                 ok$ci_upper < ok$estimate | ok$n_obs <= 0 | ok$n_clusters < 2), "Invalid uncertainty or sample size.")
  common <- ok[ok$sample_mode == "common", , drop = FALSE]
  abort_if(length(unique(common$sample_hash)) > 1L, "Common-sample specifications used different observations.")
  preferred <- results[results$preferred, , drop = FALSE]
  baseline <- "not_applicable_synthetic_demo"
  reference <- manifest$menu$reference
  if (!is.null(reference)) {
    abort_if(preferred$status != "ok" || abs(preferred$estimate - reference$estimate) > reference$tolerance,
             "Preferred specification does not reproduce the documented published baseline.")
    baseline <- "passed"
  }
  list(passed = !any(failed), expected_rows = manifest$expected_rows, completed_rows = sum(!failed),
       failed_ids = as.list(results$spec_id[failed]), baseline_reproduction = baseline,
       manifest_hash = manifest$manifest_hash, joint_inference = "not_implemented")
}
