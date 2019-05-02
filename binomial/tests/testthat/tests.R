library(testthat)


source("../../R/function.R")


context("Test for checkers")
test_that("probability range works as expected", {
  expect_equal(check_prob(0.5), TRUE)
  expect_length(check_prob(0.3), 1)
  expect_error(check_prob(2))
})
test_that("trials input is valid number of trials", {
  expect_equal(check_trials(4), TRUE)
  expect_length(check_trials(5), 1)
  expect_error(check_trials(2.1))
})
test_that("success is valid value for number of successes", {
  expect_equal(check_success(4,6), TRUE)
  expect_length(check_success(c(1,2,3,4,5),6), 1)
  expect_error(check_success(7,6), "invalid success value")
})

context("Test for summary measures")
test_that("auxillary mean works well", {
  expect_equal(aux_mean(4, 0.6), 4*0.6)
  expect_length(aux_mean(3, 0.3), 1)
  expect_equal(aux_mean(6, 0.7), 6*0.7)
})
test_that("auxillary variance works well", {
  expect_equal(aux_variance(4, 0.6), 4*0.6*0.4)
  expect_length(aux_variance(3, 0.3), 1)
  expect_equal(aux_variance(6, 0.7), 6*0.7*0.3)
})
test_that("auxillary mode works well", {
  expect_equal(aux_mode(4, 0.6), 3)
  expect_length(aux_mode(3, 0.3), 1)
  expect_equal(aux_mode(6, 0.7), 4)
})
test_that("auxillary skewness works well", {
  expect_equal(aux_skewness(4, 0.6), ((1-(2*0.6))/(4*0.6*(1-0.6))**0.5))
  expect_length(aux_skewness(3, 0.3), 1)
  expect_equal(aux_skewness(6, 0.7), ((1-(2*0.7))/(6*0.7*(1-0.7))**0.5))
})
test_that("auxillary kurtosis works well", {
  expect_equal(aux_kurtosis(4,0.6), ((1-(6*0.6*(1-0.6)))/(4*0.6*(1-0.6))))
  expect_length(aux_kurtosis(3, 0.3),  1)
  expect_equal(aux_kurtosis(6, 0.7), ((1-(6*0.7*(1-0.7)))/(6*0.7*(1-0.7))))
})

context("Test for binomial")
test_that("bin_choose works well", {
  expect_equal(bin_choose(4, 2), 6)
  expect_length(bin_choose(3, 1), 1)
  expect_equal(bin_choose(6, 3), 20)
})
test_that("bin_probability works well", {
  expect_equal(bin_probability(2, 6, 0.6), 0.13824)
  expect_length(bin_probability(3, 6, 0.3), 1)
  expect_equal(bin_probability(0:3, 5, 0.5), c(0.03125, 0.15625, 0.31250, 0.31250))
})
test_that("bin_distribution works well", {
  expect_error(bin_distribution(5, -0.5))
  expect_type(bin_distribution(3, 0.3), "list")
  expect_error(bin_distribution(-2, 0.3))
})
test_that("bin_cumulative works well", {
  expect_type(bin_cumulative(4, 0.6),"list")
  expect_error(bin_cumulative(6, -0.7))
  expect_error(bin_cumulative(-2, 0.6))
})


