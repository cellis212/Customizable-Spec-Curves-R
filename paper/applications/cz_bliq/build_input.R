#!/usr/bin/env Rscript
# Build the analysis input for the conservative-construction example: long-short
# returns of the Pastor and Stambaugh (2003) liquidity beta (BetaLiquidityPS),
# whose original-paper construction is value-weighted, under the constructions
# published by Chen and Zimmermann (2022), October 2025 release (data v2.0.0).
#
# Usage: Rscript paper/applications/cz_bliq/build_input.R RAW_DIRECTORY OUTPUT.csv
#
# RAW_DIRECTORY holds the unmodified release files listed below, which are not
# redistributed here. Hashes are checked against this table and provenance.json.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("Usage: build_input.R RAW_DIRECTORY OUTPUT.csv")
script <- sub("^--file=", "", commandArgs()[grepl("^--file=", commandArgs())][1L])
root <- normalizePath(file.path(dirname(script), "../../.."), mustWork = TRUE)
if (dir.exists(file.path(root, "renv/library")) && requireNamespace("renv", quietly = TRUE))
  renv::load(project = root, quiet = TRUE)
raw <- normalizePath(args[1L], mustWork = TRUE)
signal <- "BetaLiquidityPS"

# File, series id, and the construction it represents. The original-paper (OP)
# construction is value-weighted deciles rebalanced annually with a |price| > $5
# filter; each alternative file changes one choice. Liquidity screens replace the
# OP filter rather than adding to it.
files <- as.data.frame(do.call(rbind, list(
  c("PredictorPortsFull.csv", "op", "decile", "vw", "annual", "price5", "6cdaf0ff051222b813e6f8404d7a30b94e146b421625f202bd04473fe97a1817"),
  c("PredictorAltPorts_Deciles.zip", "deciles", "decile", "vw", "annual", "price5", "215f45d63e53a6cafd49ed0468aa5ec5d02ad524c453d6a8507f327da67e95ab"),
  c("PredictorAltPorts_DecilesEW.zip", "deciles_ew", "decile", "ew", "annual", "price5", "b49364d5ddaa3ffa789b67b0757e3e38bc4291473510751593cae3273e5ee103"),
  c("PredictorAltPorts_DecilesVW.zip", "deciles_vw", "decile", "vw", "annual", "price5", "d1790e831b621069379d5584d3114292ac52e8cdc36e44e302e880bd0c972b7c"),
  c("PredictorAltPorts_Quintiles.zip", "quintiles", "quintile", "vw", "annual", "price5", "b7d5f994ec32160a9f2183407923068d17542e7cc0a7934a769ae353e0c47d79"),
  c("PredictorAltPorts_QuintilesEW.zip", "quintiles_ew", "quintile", "ew", "annual", "price5", "2be3c54459f5de8ee1bf4a474b9a3ea7a74cb3d37b8c690ade0dff773b77a0c6"),
  c("PredictorAltPorts_QuintilesVW.zip", "quintiles_vw", "quintile", "vw", "annual", "price5", "03c2358f223670b6ce21853f2f9d86404012f6cea9a14e76d116ca05f20d5070"),
  c("PredictorAltPorts_HoldPer_1.zip", "hold1", "decile", "vw", "monthly", "price5", "1f08b5856d58c549529a22372b790905bc1b92711f5a149a4dacb2458411ece4"),
  c("PredictorAltPorts_HoldPer_3.zip", "hold3", "decile", "vw", "quarterly", "price5", "3e94f0d127b3e1808da6c40c7fc1740ad28d5dde97ff8bd1724435b5dc416fe2"),
  c("PredictorAltPorts_HoldPer_6.zip", "hold6", "decile", "vw", "semiannual", "price5", "a35e7896f46f3f08da88a4f19792047100e0e9e4eb6f369111a343a735af2ff9"),
  c("PredictorAltPorts_HoldPer_12.zip", "hold12", "decile", "vw", "annual", "price5", "dcdadcc6af1ef22e98b50987b994ea96d24eedba56ee84218b5465322e1a8502"),
  c("PredictorAltPorts_LiqScreen_ME_gt_NYSE20pct.zip", "me_nyse20", "decile", "vw", "annual", "me_nyse20", "6a889fe96fd445124305f529a1d91f19d94ca9c9f2541ff53c598c7deba61322"),
  c("PredictorAltPorts_LiqScreen_Price_gt_5.zip", "price5_screen", "decile", "vw", "annual", "price5", "7e457d36b7bcbd8c6a472d263c2df5244fcf7a2e60747e32db7c690b3620e99f"),
  c("PredictorAltPorts_LiqScreen_NYSEonly.zip", "nyse_only", "decile", "vw", "annual", "nyse_only", "eb3155f8f18427083d2597e2aa980b16f6e15ec355caa0a02c94036a56dccf6b"),
  c("PredictorAltPorts_LiqScreen_VWforce.zip", "vw_force", "decile", "vw", "annual", "price5", "53d3ae1f3de7a17d051de0125da2c42e4d4cf664faf6e4f6cfdd3b4994675a2a"))), stringsAsFactors = FALSE)
names(files) <- c("file", "series", "breakpoints", "weighting", "rebalancing", "screen", "sha256")
provenance <- jsonlite::read_json(file.path(raw, "provenance.json"))
for (i in seq_len(nrow(files))) {
  observed <- digest::digest(file = file.path(raw, files$file[i]), algo = "sha256")
  if (!identical(observed, files$sha256[i]) || !identical(observed, provenance$files[[files$file[i]]]$sha256))
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

# Drop alternative files that reproduce an earlier construction exactly.
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
data <- merge(data, files[, c("series", "breakpoints", "weighting", "rebalancing", "screen")], by = "series")
key <- unique(data[, c("breakpoints", "weighting", "rebalancing", "screen")])
if (nrow(key) != length(unique(data$series)))
  stop("Two kept series share the same construction labels; check the file-to-construction mapping.")
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
jsonlite::write_json(list(signal = signal, release = "October 2025 (v2.0.0)", raw_directory = raw,
                          dropped_duplicates = duplicates, script_sha256 = digest::digest(file = script, algo = "sha256"),
                          output_sha256 = digest::digest(file = args[2L], algo = "sha256"), rows = nrow(out),
                          built_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
                     paste0(args[2L], ".provenance.json"), pretty = TRUE, auto_unbox = TRUE)
cat("Wrote", args[2L], "\n")
