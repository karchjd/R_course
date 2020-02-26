#' Welch's t-test
#'
#' Calculates Welch's t-test
#'
#' @param x vector of observations group 1
#' @param y vector of observations group 2
#' @param mu mean difference to test against
#' @return A list with letters and numbers.
#' \itemize{
#'   \item t - t-value
#'   \item df - degrees of freedom
#'   \item p - p-value
#' }
calc_t_test <- function(x, y, mu = 0) {
  validate_t_input(x, y)


  # calculate preliminaries
  mean_x <- mean(x)
  mean_y <- mean(y)
  var_x <- var(x)
  var_y <- var(y)
  n1 <- length(x)
  n2 <- length(y)

  validate_t_post(var_x, var_y)

  # calculate t-value
  mean_difference <- mean_x - mean_y - mu
  pooled_variance <- var_x / n1 + var_y / n2
  t <- mean_difference / sqrt(pooled_variance)

  # calculate the degrees freedom
  df_numer <- (var_x / n1 + var_y / n2)^2
  v_x <- n1 - 1
  v_y <- n2 - 1
  df_denumer <- var_x^2 / (n1^2 * v_x) + var_y^2 / (n2^2 * v_y)
  df <- df_numer / df_denumer

  p <- calc_p_val(t,df)
  return(list(t = t, df = df, p = p))
}

calc_p_val <- function(t,df){
  p_val_bigger <- pt(t, df, lower.tail = TRUE)
  p_val_smaller <- pt(t, df, lower.tail = FALSE)
  p <- 2 * min(p_val_bigger, p_val_smaller)
  return(p)
}

validate_t_input <- function(x, y) {
  if (class(x) != "numeric" || class(y) != "numeric") {
    stop("x and y must be numeric")
  }

  if (length(x) == 1 || length(y) == 1) {
    stop("x and y must have at least 2 observations")
  }
}

validate_t_post <- function(var_x, var_y) {
  if (var_x == 0 || var_y == 0) {
    stop("data are essentially constant")
  }
}
