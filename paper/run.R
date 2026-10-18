#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
script <- sub("^--file=", "", commandArgs()[grepl("^--file=", commandArgs())][1L])
root <- normalizePath(file.path(dirname(script), ".."), mustWork = TRUE)
command <- if (length(args)) args[1L] else "check"
packages <- c("digest", "fixest", "ggplot2", "jsonlite", "patchwork", "renv", "yaml")

if (command == "restore") {
  if (!requireNamespace("renv", quietly = TRUE)) stop("Install renv before restoring dependencies.")
  renv::load(project = root, quiet = TRUE)
  renv::restore(project = root, lockfile = file.path(root, "renv.lock"),
                library = renv::paths$library(project = root), prompt = FALSE)
  quit(status = 0L)
}

# Use a restored project library if one exists, while permitting a normal R
# library for a first run. Check reports the versions actually in use.
if (dir.exists(file.path(root, "renv/library")) && requireNamespace("renv", quietly = TRUE))
  renv::load(project = root, quiet = TRUE)
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Missing packages: ", paste(missing, collapse = ", "), ". Run make restore.")
for (file in c("curve.R", "demo.R", "plot.R")) source(file.path(root, "paper/R", file))

if (command == "check") {
  cat(R.version.string, "\n")
  for (package in packages) cat(package, as.character(utils::packageVersion(package)), "\n")
  apps <- jsonlite::read_json(file.path(root, "paper/config/applications.json"), simplifyVector = TRUE)
  cat("Configured applications:", paste(apps$applications$id, collapse = ", "), "\n")
  quit(status = 0L)
}

if (command == "validate") {
  if (length(args) != 3L) stop("Usage: Rscript paper/run.R validate MENU.json INPUT.csv")
  menu <- read_menu(args[2L])
  data <- utils::read.csv(args[3L], stringsAsFactors = FALSE, check.names = FALSE)
  built <- validate_menu(menu, data)
  panels <- vapply(seq_len(nrow(built$grid)), function(i) resolve_spec(menu, built$grid[i, , drop = FALSE])$panel, "")
  cat("Menu", menu$version, "is valid. Nothing was estimated.\n")
  cat("Specifications:", nrow(built$grid), "| excluded combinations:", nrow(built$excluded), "\n")
  for (panel in unique(panels)) cat("  Panel", shQuote(panel), ":", sum(panels == panel), "specifications\n")
  for (d in menu_dimensions(menu)) cat("  Dimension", d$name, ":", length(d$options), "options\n")
  quit(status = 0L)
}

if (command == "plan") {
  if (length(args) != 4L) stop("Usage: Rscript paper/run.R plan MANIFEST.json INPUT.csv PLAN.csv")
  data <- utils::read.csv(args[3L], stringsAsFactors = FALSE, check.names = FALSE)
  manifest <- read_manifest(normalizePath(args[2L], mustWork = TRUE), data, root)
  utils::write.csv(execution_plan(manifest, data), args[4L], row.names = FALSE, na = "")
  cat("Wrote", manifest$expected_rows, "planned specifications to", args[4L], "\n")
  quit(status = 0L)
}

if (command == "freeze") {
  if (length(args) != 4L) stop("Usage: Rscript paper/run.R freeze MENU.json INPUT.csv MANIFEST.json")
  menu <- read_menu(args[2L])
  abort_if(menu$analysis_kind != "application", "Use the demo command for generated data.")
  data <- utils::read.csv(args[3L], stringsAsFactors = FALSE, check.names = FALSE)
  freeze_manifest(menu, data, root, args[4L])
  cat("Frozen manifest:", normalizePath(args[4L]), "\n")
  quit(status = 0L)
} else if (command == "demo") {
  menu <- read_menu(file.path(root, "paper/config/demo.json"))
  data <- make_demo_data(menu$seed)
  output_root <- Sys.getenv("SPEC_CURVES_OUTPUT_DIR", file.path(root, "local/output"))
  directory <- file.path(output_root, "synthetic-demo",
                         paste0(format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"), "-", Sys.getpid()))
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  # Estimate from the written CSV so other implementations can use identical bytes.
  input <- file.path(directory, "generated_input.csv")
  utils::write.csv(data, input, row.names = FALSE)
  data <- utils::read.csv(input, stringsAsFactors = FALSE, check.names = FALSE)
  path <- file.path(directory, "spec_curve_manifest.json")
  manifest <- freeze_manifest(menu, data, root, path)
} else if (command == "estimate") {
  if (length(args) != 4L) stop("Usage: Rscript paper/run.R estimate MANIFEST INPUT.csv OUTPUT_DIR")
  path <- normalizePath(args[2L], mustWork = TRUE)
  data <- utils::read.csv(args[3L], stringsAsFactors = FALSE, check.names = FALSE)
  directory <- args[4L]
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  manifest <- read_manifest(path, data, root)
  abort_if(manifest$menu$analysis_kind != "application", "Use the demo command for generated data.")
} else {
  stop("Unknown command. Choose check, restore, demo, validate, freeze, plan, or estimate.")
}

abort_if(file.exists(file.path(directory, "results.csv")), "Output already exists; use a fresh run directory.")
started <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
results <- run_manifest(manifest, data)
utils::write.csv(results, file.path(directory, "results.csv"), row.names = FALSE, na = "")
# Recheck frozen bytes, input, and source before validating or rendering.
manifest <- read_manifest(path, data, root)
validation <- tryCatch(validate_results(manifest, results), error = function(e) {
  list(passed = FALSE, expected_rows = manifest$expected_rows,
       completed_rows = sum(results$status == "ok"),
       failed_ids = as.list(results$spec_id[results$status == "failed"]),
       error = conditionMessage(e), baseline_reproduction = "not_passed",
       manifest_hash = manifest$manifest_hash, joint_inference = "not_implemented")
})
jsonlite::write_json(validation, file.path(directory, "validation.json"), pretty = TRUE, auto_unbox = TRUE)
run_record <- list(analysis_kind = manifest$menu$analysis_kind, started_utc = started,
                   finished_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                   manifest_hash = manifest$manifest_hash, input_hash = manifest$input_hash,
                   source_hashes = manifest$source_hashes, R_version = R.version.string,
                   input_file_sha256 = if (exists("input")) file_sha256(input) else file_sha256(args[3L]),
                   package_versions = as.list(stats::setNames(vapply(packages, function(p) as.character(utils::packageVersion(p)), ""), packages)))
jsonlite::write_json(run_record, file.path(directory, "run_record.json"), pretty = TRUE, auto_unbox = TRUE)
writeLines(capture.output(utils::sessionInfo()), file.path(directory, "sessionInfo.txt"))
abort_if(!validation$passed, paste("Validation failed:", validation$error,
                                 "Failed specifications:", paste(unlist(validation$failed_ids), collapse = ", ")))
plot_curve(manifest, results, directory)
cat("Validated", validation$completed_rows, "of", validation$expected_rows, "specifications.\n")
cat("Baseline reproduction:", validation$baseline_reproduction, "\n")
cat("Outputs:", normalizePath(directory), "\n")
