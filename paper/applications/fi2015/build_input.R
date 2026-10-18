#!/usr/bin/env Rscript
# Build the analysis input for the staggered difference-in-differences example:
# Favara and Imbs (2015, AER), Table 2 (growth in mortgages originated by banks
# and by placebo lenders on the lagged interstate branching deregulation index).
#
# Usage: Rscript paper/applications/fi2015/build_input.R RAW_DIRECTORY OUTPUT.csv
#
# RAW_DIRECTORY holds the unmodified AEA package 20121416_data.zip (openICPSR
# doi:10.3886/E112894V1). Variables follow the authors' master.do. The input is
# long by lender group (banks, placebo lenders) so the placebo is its own panel.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("Usage: build_input.R RAW_DIRECTORY OUTPUT.csv")
script <- sub("^--file=", "", commandArgs()[grepl("^--file=", commandArgs())][1L])
root <- normalizePath(file.path(dirname(script), "../../.."), mustWork = TRUE)
if (dir.exists(file.path(root, "renv/library")) && requireNamespace("renv", quietly = TRUE))
  renv::load(project = root, quiet = TRUE)
zip <- file.path(normalizePath(args[1L], mustWork = TRUE), "20121416_data.zip")
expected <- "93af8759fffddc9f7ee68e0b6e65d1cb7993b2405196ac4d7f74ab8c0ff3decd"
observed <- digest::digest(file = zip, algo = "sha256")
if (!identical(observed, expected)) stop("20121416_data.zip hash differs from the AEA package: ", observed)
read <- function(name) as.data.frame(haven::zap_formats(haven::zap_labels(haven::read_dta(
  unz(zip, file.path("20121416_1data/data", name))))))
hmda <- read("hmda.dta")
hp <- read("hp_dereg_controls.dta")
d <- merge(hmda, hp[, c("county", "year", "inter_bra", "Linter_bra", "border", "Dl_hpi", "LDl_hpi", "Dl_inc",
                        "LDl_inc", "Dl_pop", "LDl_pop")], by = c("county", "year"), all.x = TRUE)
cat("Merged county-years:", nrow(d), "| counties:", length(unique(d$county)), "| states:", length(unique(d$state_n)), "\n")

# The index is set at the state level; first deregulation year defines the adoption cohort.
stopifnot(all(tapply(d$inter_bra, paste(d$state_n, d$year), function(z) length(unique(z[!is.na(z)]))) <= 1L))
first <- tapply(seq_len(nrow(d)), d$state_n, function(i) {
  y <- d$year[i][!is.na(d$inter_bra[i]) & d$inter_bra[i] > 0]
  if (length(y)) min(y) else NA_real_
})
cohort <- unname(first[as.character(d$state_n)])
cohort_group <- ifelse(is.na(cohort), "never", ifelse(cohort <= 1996, "1995-1996",
                ifelse(cohort >= 2000, "2000-2001", as.character(cohort))))
region <- c(rep("northeast", 9), rep("midwest", 12), rep("south", 17), rep("west", 13))
names(region) <- c(9, 23, 25, 33, 44, 50, 34, 36, 42, 17, 18, 26, 39, 55, 19, 20, 27, 29, 31, 38, 46,
                   10, 11, 12, 13, 24, 37, 45, 51, 54, 1, 21, 28, 47, 5, 22, 40, 48,
                   4, 8, 16, 30, 32, 35, 49, 56, 2, 6, 15, 41, 53)
stopifnot(all(as.character(d$state_n) %in% names(region)))

common <- data.frame(county = d$county, state = d$state_n, year = d$year,
  dereg_index_lag = d$Linter_bra, dereg_index = d$inter_bra,
  dereg_any_lag = as.integer(d$Linter_bra > 0),
  Dl_inc = d$Dl_inc, LDl_inc = d$LDl_inc, Dl_pop = d$Dl_pop, LDl_pop = d$LDl_pop,
  Dl_hpi = d$Dl_hpi, LDl_hpi = d$LDl_hpi, Dl_her_v = d$Dl_her_v, LDl_her_v = d$LDl_her_v,
  border = d$border, cohort_group = cohort_group, region_year = paste(region[as.character(d$state_n)], d$year, sep = "_"),
  stringsAsFactors = FALSE)
banks <- cbind(common, lender = 1L, loans_growth = d$Dl_nloans_b, loans_growth_lag = d$LDl_nloans_b)
placebo <- cbind(common, lender = 2L, loans_growth = d$Dl_nloans_pl, loans_growth_lag = d$LDl_nloans_pl)
out <- rbind(banks, placebo)
out$row_id <- out$county * 100000 + out$year * 10 + out$lender
out <- out[order(out$lender, out$county, out$year), c("row_id", setdiff(names(out), "row_id"))]
stopifnot(!anyDuplicated(out$row_id))
cat("Rows:", nrow(out), "| years:", min(out$year), "-", max(out$year), "\n")
print(table(cohort_group = tapply(out$cohort_group, out$state, `[`, 1L)))
dir.create(dirname(args[2L]), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(out, args[2L], row.names = FALSE, na = "")
jsonlite::write_json(list(source = "AEA package 20121416_data.zip (openICPSR doi:10.3886/E112894V1)", source_sha256 = observed,
                          script_sha256 = digest::digest(file = script, algo = "sha256"),
                          output_sha256 = digest::digest(file = args[2L], algo = "sha256"), rows = nrow(out),
                          built_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
                     paste0(args[2L], ".provenance.json"), pretty = TRUE, auto_unbox = TRUE)
cat("Wrote", args[2L], "\n")
