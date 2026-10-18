#!/usr/bin/env Rscript
# Build the analysis input for the panel example with a self-described "most
# conservative" specification: Bednarek, te Kaat, Ma and Rebucci (2021, RFS),
# Table 3 (city GDP growth on the lagged GIPS spread x real-estate exposure).
#
# Usage: Rscript paper/applications/btmr2021/build_input.R RAW_DIRECTORY OUTPUT.csv
#
# RAW_DIRECTORY holds the unmodified Harvard Dataverse package
# doi:10.7910/DVN/LQPRIA (CC0), including citylevel.dta and provenance.json.
# Variable construction follows the authors' Table3.do: with city and year
# fixed effects only the interaction terms are identified.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("Usage: build_input.R RAW_DIRECTORY OUTPUT.csv")
script <- sub("^--file=", "", commandArgs()[grepl("^--file=", commandArgs())][1L])
root <- normalizePath(file.path(dirname(script), "../../.."), mustWork = TRUE)
if (dir.exists(file.path(root, "renv/library")) && requireNamespace("renv", quietly = TRUE))
  renv::load(project = root, quiet = TRUE)
raw <- normalizePath(args[1L], mustWork = TRUE)
expected <- "9a5f7c824bf74040a02a993bd386d9dd9a4c4b26b1ddf1b3b59fe045b6ec64df"
observed <- digest::digest(file = file.path(raw, "citylevel.dta"), algo = "sha256")
if (!identical(observed, expected)) stop("citylevel.dta hash differs from the Dataverse v1.0 file: ", observed)

d <- as.data.frame(haven::zap_formats(haven::zap_labels(haven::read_dta(file.path(raw, "citylevel.dta")))))
cat("Raw citylevel.dta:", nrow(d), "rows,", ncol(d), "columns\n")
# Exposure, size, and the East indicator are fixed within city; macro series within year.
for (v in c("realestate_exposure", "pop", "popdens_2008", "dummy_east"))
  stopifnot(all(tapply(d[[v]], d$kreis_id, function(z) length(unique(z))) == 1L))
for (v in c("spread_lag", "germanrate_lag", "eonia_lag", "policyrate_lag", "vix_lag"))
  stopifnot(all(tapply(d[[v]], d$year, function(z) length(unique(z))) == 1L))

state <- floor(d$kreis_id / 1e6)  # first two digits of the official municipality key
winsorize <- function(x, p = c(0.01, 0.99)) {
  q <- stats::quantile(x, p, type = 7, names = FALSE)
  pmin(pmax(x, q[1L]), q[2L])
}
out <- data.frame(
  row_id = d$kreis_id * 10000 + d$year,
  city = d$kreis_id,
  year = d$year,
  growth = d$growth,
  growth_winsorized = winsorize(d$growth),
  # Table3.do: c.spread_lag##c.realestate_exposure (focal interaction).
  spread_x_exposure = d$spread_lag * d$realestate_exposure,
  # Table3.do macro-interaction controls (columns 3-6).
  bund_x_exposure = d$germanrate_lag * d$realestate_exposure,
  eonia_x_exposure = d$eonia_lag * d$realestate_exposure,
  policy_x_exposure = d$policyrate_lag * d$realestate_exposure,
  vix_x_exposure = d$vix_lag * d$realestate_exposure,
  # Table3.do city-size controls (columns 7-8).
  spread_x_pop = d$spread_lag * d$pop,
  spread_x_popdens = d$spread_lag * d$popdens_2008,
  east = d$dummy_east,
  state = state,
  east_year = paste(d$dummy_east, d$year, sep = "_"),
  state_year = paste(state, d$year, sep = "_"),
  city_state = as.integer(state %in% c(2, 4, 11)),  # Hamburg, Bremen, Berlin
  stringsAsFactors = FALSE)
out <- out[order(out$city, out$year), ]
stopifnot(!anyDuplicated(out$row_id), !anyNA(out))
cat("Cities:", length(unique(out$city)), "| years:", min(out$year), "-", max(out$year), "| rows:", nrow(out), "\n")
cat("States:", length(unique(out$state)), "| eastern cities:", sum(tapply(out$east, out$city, max)),
    "| city-state cities:", sum(tapply(out$city_state, out$city, max)), "\n")
cat("Winsorized growth observations changed:", sum(out$growth != out$growth_winsorized), "\n")
dir.create(dirname(args[2L]), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(out, args[2L], row.names = FALSE)
jsonlite::write_json(list(source_doi = "10.7910/DVN/LQPRIA", source_sha256 = observed,
                          script_sha256 = digest::digest(file = script, algo = "sha256"),
                          output_sha256 = digest::digest(file = args[2L], algo = "sha256"), rows = nrow(out),
                          built_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
                     paste0(args[2L], ".provenance.json"), pretty = TRUE, auto_unbox = TRUE)
cat("Wrote", args[2L], "\n")
