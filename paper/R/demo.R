make_demo_data <- function(seed) {
  set.seed(seed)
  data <- expand.grid(firm = seq_len(80L), year = 2001:2012)
  n <- nrow(data)
  firm_effect <- stats::rnorm(80L)
  data$row_id <- seq_len(n)
  data$required_control <- stats::rnorm(n)
  data$size <- stats::rnorm(n)
  data$liquidity <- 0.4 * data$size + stats::rnorm(n)
  data$x <- 0.5 * data$required_control + 0.3 * data$size + stats::rnorm(n)
  data$y <- 0.15 * data$x + data$required_control + 0.5 * data$size +
    0.4 * data$liquidity + firm_effect[data$firm] + stats::rnorm(n)
  # Deliberate attrition for testing available/common samples, not empirical data.
  data$size[data$firm %% 9L == 0L] <- NA_real_
  data$liquidity[data$firm %% 7L == 0L & data$year < 2007L] <- NA_real_
  data
}
