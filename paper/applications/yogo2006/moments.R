# Moment functions for the durable-consumption model of Yogo (2006, JF), as coded
# in his nlgmm_dur.prg. Loaded by the gmm estimator family (paper/R/curve.R);
# python moments.py is the line-by-line twin.
#
# Parameters: sigma (EIS), gamma (relative risk aversion), rho (elasticity of
# substitution between nondurables and durables), alpha (durable weight), beta.
# Moments, in Yogo's order: the T-bill Euler equation, the excess-return Euler
# equations of the test assets, and the intratemporal FOC, each interacted with
# the instruments (a constant for the unconditional sets).
#
# Options: assets (ff25, ind24, bet25, all74, or ff3_conditional for Table II
# column 5), tbill and foc (include those blocks), timing (b = beginning of
# period, Yogo; e = end of period), depreciation (Yogo's 1 - delta = 0.94).

gmm_model <- local({
  parameters <- c("sigma", "gamma", "rho", "alpha", "beta")
  asset_columns <- function(assets) {
    block <- function(prefix, k) sprintf("%s_%02d", prefix, seq_len(k))
    switch(assets, ff25 = block("ff", 25L), ind24 = block("ind", 24L), bet25 = block("bet", 25L),
           all74 = c(block("ff", 25L), block("ind", 24L), block("bet", 25L)),
           ff3_conditional = block("fac", 3L), stop("Unknown test-asset set: ", assets, call. = FALSE))
  }
  consumption <- function(options) paste0(c("gc", "dc", "ldc", "price", "lprice", "infl"), "_", options$timing)
  conditional <- function(options) identical(options$assets, "ff3_conditional")
  instruments <- function(options) paste0(c("gc", "gd"), "_", options$timing)

  variables <- function(options) {
    abort_if(!options$timing %in% c("b", "e"), "Timing must be b (beginning of period) or e (end of period).")
    c(consumption(options), "tbill", "market", asset_columns(options$assets),
      if (conditional(options)) c(instruments(options), "z_dp", "z_size", "z_value", "z_yield"))
  }

  # Arrays fixed for one specification. Rows are quarters in time order; the
  # conditional set loses its first two quarters to the second-lag instruments.
  prepare <- function(frame, options) {
    abort_if(!isTRUE(options$tbill) && !isTRUE(options$foc),
             "Without the T-bill equation or the FOC the scale of the SDF, and so beta, is not identified.")
    c_vars <- consumption(options)
    lags <- if (conditional(options)) 2L else 0L
    n <- nrow(frame)
    abort_if(n <= lags + 10L, "Too few quarters for the moment conditions.")
    now <- (lags + 1L):n
    get <- function(name) frame[[name]][now]
    infl <- get(c_vars[6L])
    # The Fama-French factors are already excess returns; portfolios are not.
    assets <- as.matrix(frame[now, asset_columns(options$assets)])
    excess <- (if (conditional(options)) assets else assets - get("tbill")) / infl
    z <- if (lags) cbind(1, as.matrix(frame[seq_len(n - lags), c(instruments(options), "z_dp", "z_size", "z_value",
                                                                    "z_yield")])) else matrix(1, length(now), 1L)
    list(gc = get(c_vars[1L]), dc = get(c_vars[2L]), ldc = get(c_vars[3L]),
         rprice = get(c_vars[4L]) / get(c_vars[5L]), lprice = get(c_vars[5L]),
         rf = (1 + get("tbill")) / infl, rw = (1 + get("market")) / infl,
         excess = unname(excess), z = unname(z), tbill = isTRUE(options$tbill), foc = isTRUE(options$foc),
         depreciation = if (is.null(options$depreciation)) 0.94 else options$depreciation, rows = now)
  }

  # g_t(theta): one row per quarter. Non-finite values (the SDF is undefined at
  # sigma = 1 or rho = 1 and overflows for extreme exponents) are returned as is;
  # the estimator treats them as an infeasible parameter value.
  moments <- function(theta, prep) {
    sigma <- theta[[1L]]; gam <- theta[[2L]]; rho <- theta[[3L]]; alpha <- theta[[4L]]; beta <- theta[[5L]]
    kappa <- (1 - gam) / (1 - 1 / sigma)
    v <- (1 - alpha + alpha * prep$dc^(1 - 1 / rho))^(1 / (1 - 1 / rho))
    lv <- (1 - alpha + alpha * prep$ldc^(1 - 1 / rho))^(1 / (1 - 1 / rho))
    m <- beta^kappa * exp(prep$gc)^(-kappa / sigma) * (v / lv)^(kappa * (1 / rho - 1 / sigma)) * prep$rw^(kappa - 1)
    blocks <- list(if (prep$tbill) m * prep$rf - 1, m * prep$excess,
                   if (prep$foc) 1 - alpha / (1 - alpha) * prep$ldc^(-1 / rho) / prep$lprice -
                     prep$depreciation * m * prep$rprice)
    errors <- do.call(cbind, blocks[!vapply(blocks, is.null, logical(1))])
    # GAUSS's horizontal direct product: each error times every instrument.
    if (ncol(prep$z) == 1L) errors else
      do.call(cbind, lapply(seq_len(ncol(errors)), function(j) errors[, j] * prep$z))
  }

  list(name = "yogo_durable", parameters = parameters, variables = variables, prepare = prepare, moments = moments)
})
