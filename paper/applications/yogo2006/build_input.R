#!/usr/bin/env Rscript
# Build the analysis input for the structural example: Yogo (2006, JF), "A
# Consumption-Based Explanation of Expected Stock Returns", Table II (nonlinear
# GMM estimation of the durable-consumption model, 1951:1-2001:4).
#
# Usage: Rscript paper/applications/yogo2006/build_input.R RAW_DIRECTORY OUTPUT.csv
#
# RAW_DIRECTORY holds the unmodified CBased_Programs.zip from Yogo's research page.
# The inputs are the GAUSS matrices his nlest_dur.prg loads (data/Gauss/*.fmt);
# the public workbook CBased_Data.xls repeats the consumption-side matrix
# (Quarterly) to spreadsheet precision but not the returns.
#
# Timing: Yogo uses the "beginning of the period" convention (Campbell 2003), so
# each row already pairs the return over quarter q with consumption growth,
# durables and inflation from q to q+1. The *_e columns hold the end-of-period
# alternative: the same consumption-side series lagged one quarter.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("Usage: build_input.R RAW_DIRECTORY OUTPUT.csv")
script <- sub("^--file=", "", commandArgs()[grepl("^--file=", commandArgs())][1L])
root <- normalizePath(file.path(dirname(script), "../../.."), mustWork = TRUE)
if (dir.exists(file.path(root, "renv/library")) && requireNamespace("renv", quietly = TRUE))
  renv::load(project = root, quiet = TRUE)
zip <- file.path(normalizePath(args[1L], mustWork = TRUE), "CBased_Programs.zip")
expected <- "a707edc0ac3e8dcba2412259fa5179d5534ed8fc2d67295ccab6154808862020"
observed <- digest::digest(file = zip, algo = "sha256")
if (!identical(observed, expected)) stop("CBased_Programs.zip hash differs from Yogo's download: ", observed)

# A GAUSS matrix file: a 128-byte header, the dimensions as two little-endian
# 32-bit integers, then the values as row-major doubles.
read_fmt <- function(name) {
  con <- unz(zip, file.path("Research/Durables/data/Gauss", name), open = "rb")
  on.exit(close(con))
  bytes <- readBin(con, "raw", n = 1e7)
  stopifnot(identical(bytes[29:32], as.raw(c(0x01, 0xef, 0xcd, 0xab))))
  dims <- readBin(bytes[129:136], "integer", n = 2L, size = 4L, endian = "little")
  values <- readBin(bytes[-(1:136)], "double", n = prod(dims), size = 8L, endian = "little")
  stopifnot(length(values) == prod(dims))
  matrix(values, nrow = dims[1L], ncol = dims[2L], byrow = TRUE)
}
quarterly <- read_fmt("Quarterly.fmt")
returns <- read_fmt("Returns.fmt")
ff <- read_fmt("FF_Q.fmt")
industry <- read_fmt("VWindust.fmt")
beta <- read_fmt("VWbeta.fmt")
factors <- read_fmt("Factors_Q.fmt")
n <- nrow(quarterly)
for (m in list(returns, ff, industry, beta, factors))
  stopifnot(nrow(m) == n, identical(m[, 1L], quarterly[, 1L]), !anyNA(m))

quarter <- round((quarterly[, 3L] - quarterly[, 1L]) * 4) + 1
out <- data.frame(row_id = quarterly[, 1L] * 10 + quarter, series = "us", time = quarterly[, 3L],
                  year = quarterly[, 1L], quarter = quarter)
lag1 <- function(x) c(NA, x[-length(x)])
# Consumption side (Quarterly columns as documented in CBased_Data.xls).
consumption <- c(gc = 4L, gd = 5L, dc = 10L, ldc = 11L, price = 12L, lprice = 13L, infl = 14L)
for (name in names(consumption)) {
  out[[paste0(name, "_b")]] <- quarterly[, consumption[[name]]]
  out[[paste0(name, "_e")]] <- lag1(quarterly[, consumption[[name]]])
}
# Nominal quarterly returns: 3-month T-bill and the market (Yogo's return on wealth).
out$tbill <- returns[, 3L]
out$market <- returns[, 4L]
# Conditioning variables for Table II column 5: log dividend-price ratio, size,
# value and yield spreads (nlest_dur.prg uses Returns columns 9-12).
out$z_dp <- returns[, 9L]
out$z_size <- returns[, 10L]
out$z_value <- returns[, 11L]
out$z_yield <- returns[, 12L]
add_block <- function(out, prefix, m) {
  for (j in seq_len(ncol(m))) out[[sprintf("%s_%02d", prefix, j)]] <- m[, j]
  out
}
out <- add_block(out, "ff", ff[, 3:27])
out <- add_block(out, "ind", industry[, 3:26])
out <- add_block(out, "bet", beta[, 3:27])
out <- add_block(out, "fac", factors[, 3:5])
stopifnot(!anyDuplicated(out$row_id))
cat("Rows:", nrow(out), "| quarters:", out$year[1L], "Q", out$quarter[1L], "-", out$year[n], "Q", out$quarter[n],
    "| test portfolios:", 25 + 24 + 25, "| columns:", ncol(out), "\n")
dir.create(dirname(args[2L]), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(out, args[2L], row.names = FALSE, na = "")
jsonlite::write_json(list(source = "Yogo research page, CBased_Programs.zip (GAUSS data matrices data/Gauss/*.fmt)",
                          source_sha256 = observed, script_sha256 = digest::digest(file = script, algo = "sha256"),
                          output_sha256 = digest::digest(file = args[2L], algo = "sha256"), rows = nrow(out),
                          built_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
                     paste0(args[2L], ".provenance.json"), pretty = TRUE, auto_unbox = TRUE)
cat("Wrote", args[2L], "\n")
