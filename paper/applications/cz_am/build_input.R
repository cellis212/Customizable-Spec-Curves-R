#!/usr/bin/env Rscript
# Build the analysis input for the asset-pricing application: long-short returns
# of the Fama and French (1992) total-assets-to-market predictor (AM) under the
# original-paper construction and the alternative constructions published by
# Chen and Zimmermann (2022), April 2021 release (v1.1.0).
#
# Usage: Rscript paper/applications/cz_am/build_input.R RAW_DIRECTORY OUTPUT.csv
#
# RAW_DIRECTORY holds the unmodified release files listed below (download them
# from the release; they are not redistributed here). Hashes are checked.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("Usage: build_input.R RAW_DIRECTORY OUTPUT.csv")
script <- sub("^--file=", "", commandArgs()[grepl("^--file=", commandArgs())][1L])
root <- normalizePath(file.path(dirname(script), "../../.."), mustWork = TRUE)
if (dir.exists(file.path(root, "renv/library")) && requireNamespace("renv", quietly = TRUE))
  renv::load(project = root, quiet = TRUE)
raw <- normalizePath(args[1L], mustWork = TRUE)
signal <- "AM"

# Each file and the construction it represents. Alternative files change one
# choice relative to the original-paper (OP) construction.
files <- data.frame(
  file = c("PredictorPortsFull.csv", "PredictorAltPorts_Deciles.zip", "PredictorAltPorts_DecilesVW.zip",
           "PredictorAltPorts_Quintiles.zip", "PredictorAltPorts_QuintilesVW.zip",
           "PredictorAltPorts_HoldPer_1.zip", "PredictorAltPorts_HoldPer_3.zip",
           "PredictorAltPorts_HoldPer_6.zip", "PredictorAltPorts_HoldPer_12.zip",
           "PredictorAltPorts_LiqScreen_ME_gt_NYSE20pct.zip", "PredictorAltPorts_LiqScreen_Price_gt_5.zip",
           "PredictorAltPorts_LiqScreen_NYSEonly.zip", "PredictorAltPorts_LiqScreen_VWforce.zip"),
  series = c("op", "deciles", "deciles_vw", "quintiles", "quintiles_vw", "hold1", "hold3", "hold6", "hold12",
             "me_nyse20", "price5", "nyse_only", "vw_force"),
  sha256 = c("f51caf0e55c13e4defb915d24c7ee4357d61bd63802af023458b78381b581ceb",
             "c86c76bdeb625a2a1f4ada3b837f5fff27d09c8084ac3a96ef23d8b6ad2ec468",
             "da46f9d5090ce0ad2d0db441f2064ef6238b7e5ba2a36dc925f14d64123ea2c4",
             "4996bd5c6ba9aeb457b7ebd5fcc5e3bb215a868a9536f1ddade7e63a06dfa470",
             "b8bae8c2ae1a9ef67e537ed05f15237aea50527aa47df6d6cefc63c49cc14e49",
             "4754b26312fef75f696e2d16bfca93b58fc1276e4934756c0d781bd9d7bc753d",
             "673ff679f9b81cea5eab16793576ade6c9d20e26e06b937a5de587874c53aff1",
             "8c712b591ad931b62441262aedaab6df3d5f059895739fd46bb8d97f2aabc91a",
             "ff3dcc5770a575aed86d53308b4a5c66c903cb6d1bf41f1b772c6525ca10bc82",
             "142b2f0a77938ac5274e7bf898e9f67b5c2604b2e13d94fb26eb2d3536626328",
             "a7121cbb4a2353a32ce1c6882f4eb02503d777607b1d0418ba414cc3c52d58f4",
             "57f9a1242aa7b3d66edfc43c5135cd17d365ec1657c588ffb6ce7d8984421a59",
             "4718289600bcc2a2795415e3af1e916fd9aa7277c373b7b8ccaeee6e885c2ac1"),
  stringsAsFactors = FALSE)
provenance <- jsonlite::read_json(file.path(raw, "provenance.json"))
for (i in seq_len(nrow(files))) {
  observed <- digest::digest(file = file.path(raw, files$file[i]), algo = "sha256")
  recorded <- provenance$files[[files$file[i]]]$sha256
  if (!identical(observed, files$sha256[i]) || !identical(observed, recorded))
    stop("Hash mismatch for ", files$file[i], ": ", observed)
}

# Stream each file and keep only the selected signal's long-short rows.
read_signal <- function(path) {
  connection <- if (grepl("[.]zip$", path)) {
    inner <- utils::unzip(path, list = TRUE)$Name
    inner <- inner[grepl("[.]csv$", inner)]
    stopifnot(length(inner) == 1L)
    unz(path, inner)
  } else file(path)
  open(connection, "r")
  on.exit(close(connection))
  header <- readLines(connection, n = 1L)
  kept <- character()
  repeat {
    lines <- readLines(connection, n = 500000L)
    if (!length(lines)) break
    kept <- c(kept, lines[startsWith(lines, paste0(signal, ",")) | startsWith(lines, paste0('"', signal, '",'))])
  }
  out <- utils::read.csv(text = c(header, kept), stringsAsFactors = FALSE)
  out[out$port == "LS", , drop = FALSE]
}

parts <- lapply(seq_len(nrow(files)), function(i) {
  part <- read_signal(file.path(raw, files$file[i]))
  cat(sprintf("%-48s %6d long-short months\n", files$file[i], nrow(part)))
  part$series <- files$series[i]
  part
})
data <- do.call(rbind, parts)
stopifnot(all(data$signalname == signal))
data$date <- as.Date(data$date)
data$yyyymm <- as.integer(format(data$date, "%Y")) * 100L + as.integer(format(data$date, "%m"))
data$year <- as.integer(format(data$date, "%Y"))

# Some alternative files reproduce another construction exactly for this
# signal (for AM the OP construction is equal-weighted quintiles rebalanced
# annually). Keep the first series in file order and drop exact duplicates.
same_series <- function(a, b) {
  x <- merge(data[data$series == a, c("yyyymm", "ret")], data[data$series == b, c("yyyymm", "ret")],
             by = "yyyymm", all = TRUE)
  !anyNA(x$ret.x) && !anyNA(x$ret.y) && max(abs(x$ret.x - x$ret.y)) < 1e-10
}
duplicates <- list()
for (j in seq_len(nrow(files))[-1L]) for (i in seq_len(j - 1L)) {
  if (files$series[i] %in% names(duplicates)) next
  if (same_series(files$series[i], files$series[j])) {
    duplicates[[files$series[j]]] <- files$series[i]
    cat(sprintf("Exact duplicate: %s reproduces %s; dropped\n", files$series[j], files$series[i]))
    break
  }
}
data <- data[!data$series %in% names(duplicates), ]

# Construction attributes for the menu's factorial labels.
attributes <- data.frame(
  series = files$series,
  # OP for AM uses quintiles (verified above: the Quintiles file equals OP).
  breakpoints = c("quintile", "decile", "decile", "quintile", "quintile", rep("quintile", 8)),
  weighting = c("ew", "ew", "vw", "ew", "vw", "ew", "ew", "ew", "ew", "ew", "ew", "ew", "vw"),
  rebalancing = c("annual", "annual", "annual", "annual", "annual", "monthly", "quarterly", "semiannual", "annual",
                  "annual", "annual", "annual", "annual"),
  screen = c("none", "none", "none", "none", "none", "none", "none", "none", "none", "me_nyse20", "price5",
             "nyse_only", "none"),
  stringsAsFactors = FALSE)
data <- merge(data, attributes, by = "series")
key <- unique(data[, c("breakpoints", "weighting", "rebalancing", "screen")])
stopifnot(nrow(key) == length(unique(data$series)))
data$min_leg <- pmin(data$Nlong, data$Nshort)
data <- data[order(match(data$series, files$series), data$yyyymm), ]
data$row_id <- seq_len(nrow(data))
out <- data[, c("row_id", "series", "breakpoints", "weighting", "rebalancing", "screen", "yyyymm", "year",
                "ret", "Nlong", "Nshort", "min_leg")]
names(out)[names(out) == "Nlong"] <- "n_long"
names(out)[names(out) == "Nshort"] <- "n_short"
cat("Rows:", nrow(out), "| series:", length(unique(out$series)), "| months:", min(out$yyyymm), "-", max(out$yyyymm), "\n")
cat("Missing returns:", sum(is.na(out$ret)), "| months with fewer than 20 stocks in a leg:",
    sum(out$min_leg < 20, na.rm = TRUE), "\n")
cat("Series kept:", paste(unique(out$series), collapse = ", "), "\n")
dir.create(dirname(args[2L]), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(out, args[2L], row.names = FALSE)
jsonlite::write_json(list(signal = signal, release = "April 2021 (v1.1.0)", raw_directory = raw,
                          dropped_duplicates = duplicates, script_sha256 = digest::digest(file = script, algo = "sha256"),
                          output_sha256 = digest::digest(file = args[2L], algo = "sha256"), rows = nrow(out),
                          built_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
                     paste0(args[2L], ".provenance.json"), pretty = TRUE, auto_unbox = TRUE)
cat("Wrote", args[2L], "\n")
