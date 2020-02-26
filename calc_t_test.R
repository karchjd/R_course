library(testthat)
#' Welch's t-test
#'
#' Calculates the Welch t-test
#'
#' @param x vector of observations group 1
#' @param y vector of observations group 2
#' @details t is the t value
#'          df is the degrees of freedom
#'          p is the p-value
calc_t_test <- function(x, y) {
  validate_t_input(x,y)
  
  
  # calculate prelim.
  mean_x <- julian::mean(x)
  mean_y <- mean(y)
  var_x <- var(x)
  var_y <- var(y)
  n1 <- length(x)
  n2 <- length(y)

  # calculate t-value
  
  mean_difference <- mean_x - mean_y
  pooled_variance <- var_x / n1 + var_y / n2
  t <- mean_difference / sqrt(pooled_variance)

  # calculate the degrees freedom
  df_numer <- (var_x / n1 + var_y / n2)^2
  v_x <- n1 - 1
  v_y <- n2 - 1
  df_denumer <- var_x^2 / (n1^2 * v_x) + var_y^2 / (n2^2 * v_y)
  df <- df_numer / df_denumer

  # calculate p-value
  p_val_bigger <- pt(t, df, lower.tail = TRUE)
  p_val_smaller <- pt(t, df, lower.tail = FALSE)
  p <- 2 * min(p_val_bigger, p_val_smaller)

  return(list(t = t, df = df, p = p))
}

calc_mean <- function(x) {
  current_sum <- 0
  for (i in 1:length(x)) {
    current_sum <- current_sum + x[i]
  }
  the_mean <- current_sum / length(x)
  return(the_mean)
}

calc_variance <- function(x) {
  current_SS <- 0
  our_mean <- calc_mean(x)
  for (i in 1:length(x)) {
    current_SS <- current_SS + (x[i] - our_mean)^2
  }
  the_var <- current_SS / (length(x)-1)
  return(the_var)
}

validate_t_input <- function(x,y){
  if(class(x) != "numeric" || class(y) != "numeric"){
    stop("x and y must be numeric")
  }
  
  if(length(x) == 1 || length(y) == 1){
    stop("x and y must have at least 2 observations")
  }
}

x <- rnorm(10^7)
y <- rnorm(10^7)

our_res <- calc_t_test(x, y)
r_res <- t.test(x, y)

expect_equivalent(our_res$df,r_res$parameter)
expect_equivalent(our_res$p,r_res$p.value)
expect_equivalent(our_res$t,r_res$statistic)

expect_equivalent(calc_mean(x),mean(x))
expect_equivalent(calc_variance(x),var(x))

expect_error(calc_t_test("Hallo","Toto"),"x and y must be numeric")
expect_error(calc_t_test(10,5),"x and y must have at least 2 observations")
expect_error(calc_t_test(julian,david),"x and y must be numeric")

