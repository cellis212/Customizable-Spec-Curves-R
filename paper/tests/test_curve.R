args <- commandArgs()
script <- sub("^--file=", "", args[grepl("^--file=", args)][1L])
root <- normalizePath(file.path(dirname(script), "../.."))
if (dir.exists(file.path(root, "renv/library")) && requireNamespace("renv", quietly = TRUE))
  renv::load(project = root, quiet = TRUE)
for (file in c("curve.R", "demo.R")) source(file.path(root, "paper/R", file))

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
menu <- jsonlite::read_json(file.path(root, "paper/config/demo.json"), simplifyVector = TRUE)
data <- make_demo_data(menu$seed)
path <- file.path(directory, "manifest.json")
manifest <- freeze_manifest(menu, data, root, path)
results <- run_manifest(manifest, data)
validation <- validate_results(manifest, results)
check(validation$passed, "Synthetic grid should complete.")
check(nrow(results) == 6L && all(results$status == "ok"), "All six planned rows must exist.")
check(all(grepl("required_control", results$formula)), "Required controls must never become optional.")
check(all(results$clustering == "firm"), "Clustering must remain fixed.")
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
wrong_labels$control_block[1L] <- "unexpected"
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
