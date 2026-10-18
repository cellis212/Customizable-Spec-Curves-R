#!/usr/bin/env Rscript
# Build the analysis input for the Beck, Levine and Levkov (2010) application.
#
# Usage: Rscript paper/applications/bll2010/build_input.R PATH/TO/bbb.zip OUTPUT.csv
#
# bbb.zip is the unmodified replication archive from DataverseNL
# (doi:10.34894/B1K9OU, CC BY 4.0). It is read, never modified, and is not
# redistributed with this repository. Variable construction follows the
# authors' TableII.do; alternatives used by the menu are added explicitly.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("Usage: build_input.R PATH/TO/bbb.zip OUTPUT.csv")
script <- sub("^--file=", "", commandArgs()[grepl("^--file=", commandArgs())][1L])
root <- normalizePath(file.path(dirname(script), "../../.."), mustWork = TRUE)
if (dir.exists(file.path(root, "renv/library")) && requireNamespace("renv", quietly = TRUE))
  renv::load(project = root, quiet = TRUE)

archive <- normalizePath(args[1L], mustWork = TRUE)
expected <- "f49d78db6be4e284c5b53d52949f07f072884d080df79780bd61d585d0093cd9"
observed <- digest::digest(file = archive, algo = "sha256")
if (!identical(observed, expected)) stop("Archive hash differs from the DataverseNL v7.0 file: ", observed)

directory <- tempfile("bll-")
utils::unzip(archive, files = "bbb/macro_workfile.dta", exdir = directory)
raw <- haven::zap_formats(haven::zap_labels(haven::read_dta(file.path(directory, "bbb/macro_workfile.dta"))))
raw <- as.data.frame(raw)
cat("Raw macro_workfile:", nrow(raw), "rows,", ncol(raw), "columns\n")

# Census Bureau regions (Census Regions and Divisions of the United States).
regions <- list(
  northeast = c(9, 23, 25, 33, 44, 50, 34, 36, 42),
  midwest = c(17, 18, 26, 39, 55, 19, 20, 27, 29, 31, 38, 46),
  south = c(10, 11, 12, 13, 24, 37, 45, 51, 54, 1, 21, 28, 47, 5, 22, 40, 48),
  west = c(4, 8, 16, 30, 32, 35, 49, 56, 2, 6, 15, 41, 53))
region_of <- function(fips) {
  out <- rep(NA_character_, length(fips))
  for (name in names(regions)) out[fips %in% regions[[name]]] <- name
  out
}

stopifnot(all(raw$`_intra` == as.integer(raw$wrkyr > raw$branch_reform)))
data <- data.frame(
  row_id = raw$statefip * 10000L + raw$wrkyr,
  state = as.integer(raw$statefip),
  year = as.integer(raw$wrkyr),
  region = region_of(raw$statefip),
  reform_year = as.integer(raw$branch_reform),
  # TableII.do: _intra is 1 strictly after the reform year.
  deregulated = as.integer(raw$`_intra`),
  # Alternative coding: the reform year itself counts as treated.
  deregulated_from_reform_year = as.integer(raw$wrkyr >= raw$branch_reform),
  # TableII.do outcome transformations.
  log_gini = log(raw$gini),
  logistic_gini = log(raw$gini / (1 - raw$gini)),
  log_theil = log(raw$theil),
  # TableII.do controls (Panel B).
  gsp_pc_growth = raw$gsp_pc_growth,
  prop_blacks = raw$prop_blacks,
  prop_dropouts = raw$prop_dropouts,
  prop_female_headed = raw$prop_female_headed,
  unemploymentrate = raw$unemploymentrate,
  stringsAsFactors = FALSE)
data$region_year <- paste(data$region, data$year, sep = "_")
data$reformed_before_1976 <- as.integer(data$reform_year < 1976)
data <- data[order(data$state, data$year), ]

stopifnot(!anyNA(data$region), !anyDuplicated(data$row_id))
cat("States:", length(unique(data$state)), "| years:", min(data$year), "-", max(data$year),
    "| rows:", nrow(data), "\n")
cat("Regions (states):", paste(names(table(unique(data[, c("state", "region")])$region)),
    table(unique(data[, c("state", "region")])$region), collapse = ", "), "\n")
cat("States reformed before 1976:", sum(unique(data[, c("state", "reformed_before_1976")])$reformed_before_1976), "\n")
cat("Rows treated (BLL coding):", sum(data$deregulated), "| (reform-year coding):",
    sum(data$deregulated_from_reform_year), "\n")
cat("Missing values per variable:\n")
print(vapply(data, function(x) sum(is.na(x)), integer(1)))

dir.create(dirname(args[2L]), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(data, args[2L], row.names = FALSE)
provenance <- list(source_archive = archive, source_sha256 = observed,
                   source_doi = "10.34894/B1K9OU", built_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                   script_sha256 = digest::digest(file = script, algo = "sha256"),
                   output_sha256 = digest::digest(file = args[2L], algo = "sha256"), rows = nrow(data))
jsonlite::write_json(provenance, paste0(args[2L], ".provenance.json"), pretty = TRUE, auto_unbox = TRUE)
cat("Wrote", args[2L], "\n")
