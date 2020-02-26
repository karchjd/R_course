library(testthat)
this_dir <- dirname(parent.frame(2)$ofile)
setwd(this_dir)
source("calc_t_test.R")
x <- rnorm(10^3)
y <- rnorm(10^3)

our_res <- calc_t_test(x, y)
r_res <- t.test(x, y)

expect_equivalent(our_res$df, r_res$parameter)
expect_equal(our_res$p, r_res$p.value)
expect_equivalent(our_res$t, r_res$statistic)

our_res <- calc_t_test(x, y, mu = 4)
r_res <- t.test(x, y, mu = 4)
expect_equal(our_res$p, r_res$p.value)

expect_error(calc_t_test("Hallo", "Toto"), "x and y must be numeric")
expect_error(calc_t_test(10, 5), "x and y must have at least 2 observations")
expect_error(calc_t_test(julian, david), "x and y must be numeric")
expect_error(calc_t_test(rep(1, 10), rep(2, 10)),
             "data are essentially constant")
