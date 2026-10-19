#!/usr/bin/env Rscript
# Reproduction gate, part 1 (no optimizer): at the second-step parameter vectors
# Yogo's code saved for each Table II column, our moments and VARHAC must give
# the printed J statistics and standard errors. Part 2 is the preferred row of
# the frozen grid (gamma = 191.438 from the saved first-step vector).
#
# Usage: Rscript paper/applications/yogo2006/check_published.R RAW_DIRECTORY INPUT.csv OUTPUT.json

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) stop("Usage: check_published.R RAW_DIRECTORY INPUT.csv OUTPUT.json")
script <- sub("^--file=", "", commandArgs()[grepl("^--file=", commandArgs())][1L])
root <- normalizePath(file.path(dirname(script), "../../.."), mustWork = TRUE)
if (dir.exists(file.path(root, "renv/library")) && requireNamespace("renv", quietly = TRUE))
  renv::load(project = root, quiet = TRUE)
options(speccurve.root = root)
source(file.path(root, "paper/R/curve.R"))
zip <- file.path(normalizePath(args[1L], mustWork = TRUE), "CBased_Programs.zip")
stopifnot(identical(digest::digest(file = zip, algo = "sha256"),
                    "a707edc0ac3e8dcba2412259fa5179d5534ed8fc2d67295ccab6154808862020"))
data <- utils::read.csv(args[2L], stringsAsFactors = FALSE)
menu <- read_menu(file.path(root, "paper/applications/yogo2006/menu.json"))
model <- gmm_load(menu)

read_fmt <- function(name) {
  con <- unz(zip, file.path("Research/Durables", name), open = "rb")
  on.exit(close(con))
  bytes <- readBin(con, "raw", n = 1e7)
  dims <- readBin(bytes[129:136], "integer", n = 2L, size = 4L, endian = "little")
  matrix(readBin(bytes[-(1:136)], "double", n = prod(dims), size = 8L, endian = "little"), dims[1L], dims[2L], byrow = TRUE)
}
# Table II as Yogo's tables.prg printed it (tables/nonlin.txt): parameter and SE
# rows, then the two Wald tests, then the J test and its p-value.
lines <- readLines(unz(zip, "Research/Durables/tables/nonlin.txt"))
numbers <- lapply(lines, function(l) as.numeric(regmatches(l, gregexpr("-?[0-9]+[.][0-9]+", l))[[1L]]))
printed <- list(theta = do.call(rbind, numbers[c(1, 3, 5, 7, 9)]), se = do.call(rbind, numbers[c(2, 4, 6, 8, 10)]),
                j = numbers[[15L]], j_p = numbers[[16L]])

columns <- list(list(dir = "FF", assets = "ff25"), list(dir = "indust", assets = "ind24"),
                list(dir = "beta", assets = "bet25"), list(dir = "all", assets = "all74"),
                list(dir = "instrum", assets = "ff3_conditional"))
vcov <- list(type = "varhac", max_lag = 1, criterion = "aic")
bounds <- gmm_bounds(menu)
frame <- data[order(data$time), ]
out <- lapply(seq_along(columns), function(k) {
  column <- columns[[k]]
  saved <- read_fmt(file.path("results", column$dir, "nlest_dur.fmt"))
  theta <- stats::setNames(saved[1:5, 3L], bounds$name)
  options <- list(assets = column$assets, tbill = TRUE, foc = TRUE, timing = "b", depreciation = 0.94)
  prepared <- model$prepare(frame, options)
  n <- length(prepared$rows)
  mean_g <- function(t) colMeans(model$moments(t, prepared))
  s <- gmm_long_run(model$moments(theta, prepared), vcov)
  gbar <- mean_g(theta)
  grad <- gmm_jacobian(mean_g, theta, bounds$lower, bounds$upper, gbar)
  se <- sqrt(diag(solve(t(grad) %*% solve(s, grad)) / n))
  j <- n * drop(t(gbar) %*% solve(s, gbar))
  df <- length(gbar) - length(theta)
  j_p <- stats::pchisq(j, df, lower.tail = FALSE)
  gaps <- c(abs(se - printed$se[, k]), abs(j - printed$j[k]), abs(j_p - printed$j_p[k]))
  list(column = k, test_assets = column$assets, quarters = n, moments = length(gbar),
       theta = as.list(theta), se = as.list(stats::setNames(se, bounds$name)), j = j, j_df = df, j_p_value = j_p,
       printed_se = as.list(stats::setNames(printed$se[, k], bounds$name)), printed_j = printed$j[k], printed_j_p = printed$j_p[k],
       saved_se = as.list(stats::setNames(saved[1:5, 4L], bounds$name)), saved_j = saved[8L, 3L],
       max_relative_gap_to_saved = max(abs(c(se, j) / c(saved[1:5, 4L], saved[8L, 3L]) - 1)),
       max_abs_gap_to_printed = max(gaps), passed = all(gaps <= 0.0005 + 1e-9))
})
result <- list(gate = "Yogo (2006) Table II: J statistics and standard errors at the saved second-step vectors",
               tolerance = "absolute 0.0005 against the printed three decimals", columns = out,
               passed = all(vapply(out, function(x) x$passed, logical(1))))
jsonlite::write_json(result, args[3L], pretty = TRUE, auto_unbox = TRUE, digits = NA)
for (x in out) cat(sprintf("Column %d (%s, T = %d, %d moments): J %.3f (printed %.3f), SE(gamma) %.3f (printed %.3f), max gap %.2e, rel. gap to saved %.1e %s\n",
                           x$column, x$test_assets, x$quarters, x$moments, x$j, x$printed_j, x$se$gamma, x$printed_se$gamma,
                           x$max_abs_gap_to_printed, x$max_relative_gap_to_saved, if (x$passed) "PASS" else "FAIL"))
if (!result$passed) stop("Table II J statistics or standard errors not reproduced.")
cat("Gate part 1 passed:", args[3L], "\n")
