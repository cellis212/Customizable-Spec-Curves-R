# Linear moment model for the gmm tests, E[z_t (y_t - a - b x_t)] = 0, so the
# identity and two-step estimates have closed forms. Twin of
# python/tests/gmm_linear_moments.py. Option broken = TRUE makes every moment
# missing, to test that failed rows are kept with their reason.

gmm_model <- list(
  name = "linear_iv", parameters = c("a", "b"),
  variables = function(options) c("y", "x", if (identical(options$instruments, "z1")) "z1" else c("z1", "z2")),
  prepare = function(frame, options) {
    z <- cbind(1, as.matrix(frame[, if (identical(options$instruments, "z1")) "z1" else c("z1", "z2"), drop = FALSE]))
    list(y = frame$y, x = frame$x, z = unname(z), broken = isTRUE(options$broken), rows = seq_len(nrow(frame)))
  },
  moments = function(theta, prep) {
    e <- prep$y - theta[[1L]] - theta[[2L]] * prep$x
    if (prep$broken) e <- e * NA_real_
    e * prep$z
  })
