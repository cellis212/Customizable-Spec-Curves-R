#!/usr/bin/env Rscript
# Build the analysis input for the regression-discontinuity example: Cellini,
# Ferreira and Rothstein (2010, QJE), Table IV Panel A (intent-to-treat effects of
# passing a school bond measure on district spending, dynamic RD).
#
# Usage: Rscript paper/applications/cfr2010/build_input.R RAW_DIRECTORY OUTPUT.csv
#
# RAW_DIRECTORY holds the authors' archive files recursivepanel.dta.zip and
# cfr_archive_081511_small.zip. The sample and regressors follow fiscaltab_rf.do:
# general-obligation bond measures, relative years -2 to 6; each row belongs to
# the stack of one measure (refid). Window, kernel, donut, and threshold choices
# refer to the vote margin of that measure (vote share minus required share).

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("Usage: build_input.R RAW_DIRECTORY OUTPUT.csv")
script <- sub("^--file=", "", commandArgs()[grepl("^--file=", commandArgs())][1L])
root <- normalizePath(file.path(dirname(script), "../../.."), mustWork = TRUE)
if (dir.exists(file.path(root, "renv/library")) && requireNamespace("renv", quietly = TRUE))
  renv::load(project = root, quiet = TRUE)
raw <- normalizePath(args[1L], mustWork = TRUE)
zip <- file.path(raw, "recursivepanel.dta.zip")
expected <- "e99f583d5537f48b3dcee9e6207354e000ab4ce740a1f508c5f59727a48228d8"
observed <- digest::digest(file = zip, algo = "sha256")
if (!identical(observed, expected)) stop("recursivepanel.dta.zip hash differs from the authors' archive: ", observed)
d <- as.data.frame(haven::zap_formats(haven::zap_labels(haven::read_dta(unz(zip, "recursivepanel.dta")))))
cat("Raw recursivepanel.dta:", nrow(d), "rows\n")
d <- d[d$gobond == 1 & d$dyear >= -2 & d$dyear <= 6, ]
stopifnot(!anyDuplicated(d[, c("refid", "dyear")]), all(table(d$refid[d$dyear == 0]) == 1L))

# Vote margin of the measure that defines each stack (relative year 0 terms).
focal <- d[d$dyear == 0, c("refid", "percent_0", "req_0")]
focal$margin <- focal$percent_0 - focal$req_0
d$focal_margin <- focal$margin[match(d$refid, focal$refid)]
d$focal_req <- focal$req_0[match(d$refid, focal$refid)]
tau <- 1:6
out <- data.frame(row_id = d$refid * 100 + d$dyear + 2, refid = d$refid, district = d$newleaid, year = d$year,
                  dyear = d$dyear, capital_outlays_pp = d$tcapout_pp, total_expenditure_pp = d$totalexp_pp,
                  instructional_pp = d$tcurinst_pp, stringsAsFactors = FALSE)
for (t in tau) {
  out[[paste0("win_", t)]] <- d[[paste0("win_", t)]]
  out[[paste0("req_", t)]] <- d[[paste0("req_", t)]]
  out[[paste0("percent_", t)]] <- d[[paste0("percent_", t)]]
  out[[paste0("percent2_", t)]] <- d[[paste0("percent2_", t)]]
  out[[paste0("percent3_", t)]] <- d[[paste0("percent3_", t)]]
  # Margin relative to the measure's own threshold (zero when no measure), and its slope above the threshold.
  out[[paste0("margin_", t)]] <- d[[paste0("percent_", t)]] - d[[paste0("req_", t)]]
  out[[paste0("win_margin_", t)]] <- d[[paste0("win_", t)]] * out[[paste0("margin_", t)]]
}
out$focal_margin <- d$focal_margin
out$abs_margin <- abs(d$focal_margin)
out$two_thirds <- as.integer(d$focal_req > 60)
for (h in c(20, 10, 5)) out[[paste0("tri_", h)]] <- ifelse(out$abs_margin < h, 1 - out$abs_margin / h, NA_real_)
out <- out[order(out$refid, out$dyear), ]
stopifnot(!anyDuplicated(out$row_id))
cat("Rows:", nrow(out), "| measures:", length(unique(out$refid)), "| districts:", length(unique(out$district)),
    "| two-thirds measures:", sum(tapply(out$two_thirds, out$refid, `[`, 1L)), "\n")
cat("Measures within 20/10/5 points:", vapply(c(20, 10, 5), function(h) sum(tapply(out$abs_margin, out$refid, `[`, 1L) < h), 1),
    "| within 1 point:", sum(tapply(out$abs_margin, out$refid, `[`, 1L) < 1), "\n")
dir.create(dirname(args[2L]), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(out, args[2L], row.names = FALSE, na = "")
jsonlite::write_json(list(source = "Cellini-Ferreira-Rothstein replication archive (recursivepanel.dta.zip)",
                          source_sha256 = observed, script_sha256 = digest::digest(file = script, algo = "sha256"),
                          output_sha256 = digest::digest(file = args[2L], algo = "sha256"), rows = nrow(out),
                          built_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
                     paste0(args[2L], ".provenance.json"), pretty = TRUE, auto_unbox = TRUE)
cat("Wrote", args[2L], "\n")
