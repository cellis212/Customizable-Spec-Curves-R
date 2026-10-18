#!/usr/bin/env Rscript
# Build the analysis input for the instrumental-variables example: Burgess and
# Pande (2005, AER), Tables 3-5 (rural poverty on rural bank branch expansion,
# instrumented by the 1977-1990 branch-licensing rule).
#
# Usage: Rscript paper/applications/bp2005/build_input.R RAW_DIRECTORY OUTPUT.csv
#
# RAW_DIRECTORY holds the unmodified AEA package june05_data_burgess.zip
# (openICPSR doi:10.3886/E112313V1). Sample restrictions follow the authors'
# burgesspandeaerregressions.do: years to 2000, Haryana before 1965 dropped.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("Usage: build_input.R RAW_DIRECTORY OUTPUT.csv")
script <- sub("^--file=", "", commandArgs()[grepl("^--file=", commandArgs())][1L])
root <- normalizePath(file.path(dirname(script), "../../.."), mustWork = TRUE)
if (dir.exists(file.path(root, "renv/library")) && requireNamespace("renv", quietly = TRUE))
  renv::load(project = root, quiet = TRUE)
zip <- file.path(normalizePath(args[1L], mustWork = TRUE), "june05_data_burgess.zip")
expected <- "caed05e13c700322192d827afe9fdd175e4a7ea6fa8acdbbce4560f22f3b059f"
observed <- digest::digest(file = zip, algo = "sha256")
if (!identical(observed, expected)) stop("june05_data_burgess.zip hash differs from the AEA package: ", observed)
d <- as.data.frame(haven::zap_formats(haven::zap_labels(haven::read_dta(unz(zip, "burgesspandeaerfinal.dta")))))
cat("Raw burgesspandeaerfinal.dta:", nrow(d), "rows,", ncol(d), "columns\n")
d <- d[d$year <= 2000 & !(d$state == 5 & d$year < 1965), ]

controls <- c("popt61", "popt77", "popd77", "popt90", "popdum90", "rursht61", "rursht77", "rurshd77", "rursht90",
              "rurshd90", "stint61", "stint77", "stin77d", "stint90", "stindum90")
policy <- c("clre", "hcapsh2", "odevsh2")
politics <- c("pcona", "pjan", "phdlft", "preg", "phind")
keep <- c("state", "year", "h1", "runbfor", "rurcrsh", "rursavsh", "ubt61", "ubt77", "ubt90", "ubdum77", "ubdum90",
          controls, policy, politics)
out <- d[, keep]
out$survey_year <- as.integer(!is.na(d$round))
out$row_id <- out$state * 10000 + out$year
out <- out[order(out$state, out$year), c("row_id", setdiff(names(out), "row_id"))]
stopifnot(!anyDuplicated(out$row_id))
states <- unique(d[, c("state", "statenm")])
cat("Rows:", nrow(out), "| states:", nrow(states), "| years:", min(out$year), "-", max(out$year),
    "| survey years:", sum(out$survey_year), "\n")
print(states[order(states$state), ], row.names = FALSE)
dir.create(dirname(args[2L]), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(out, args[2L], row.names = FALSE, na = "")
jsonlite::write_json(list(source = "AEA package june05_data_burgess.zip (openICPSR doi:10.3886/E112313V1)",
                          source_sha256 = observed, script_sha256 = digest::digest(file = script, algo = "sha256"),
                          output_sha256 = digest::digest(file = args[2L], algo = "sha256"), rows = nrow(out),
                          built_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
                     paste0(args[2L], ".provenance.json"), pretty = TRUE, auto_unbox = TRUE)
cat("Wrote", args[2L], "\n")
