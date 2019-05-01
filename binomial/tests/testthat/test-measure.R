context("Summary measures arguments")

# aux_mean

test_that("aux_mean works with valid inputs", {
  trials = 5
  prob = 0.3
  expect_equal(aux_mean(trials, prob), 1.5)
  expect_length(aux_mean(trials, prob), 1)
  expect_type(aux_mean(trials, prob), "double")
})

# aux_variance

test_that("aux_variance works with valid inputs", {
  trials = 5
  prob = 0.7
  expect_equal(aux_variance(trials, prob), 5*0.7*(1 - 0.7))
  expect_length(aux_variance(trials, prob), 1)
  expect_type(aux_variance(trials, prob), "double")
})

# aux_mode

test_that("aux_mode works with valid inputs", {
  n = 5
  p = 0.4
  n1 = 7
  p1 = 0.5
  expect_equal(aux_mode(n, p), floor(n*p + p))
  expect_equal(aux_mode(n1, p1), c((n1*p1 + p1), (n1*p1 + p1)- 1))
  expect_length(aux_mode(n, p), 1)
  expect_length(aux_mode(n1, p1), 2)
  expect_type(aux_mode(n, p), "double")
  expect_type(aux_mode(n1, p1), "double")
})

# aux_skewness

test_that("aux_skewness works as expected", {
  trials = 5
  prob = 0.7
  expect_equal(aux_skewness(trials, prob), (1-2*0.7)/sqrt(5*0.7*0.3))
  expect_length(aux_skewness(trials, prob), 1)
  expect_type(aux_skewness(trials, prob), "double")
})

# aux_kurtosis

test_that("aux_kurtosis works as expected", {
  trials = 5
  prob = 0.6
  expect_equal(aux_kurtosis(trials, prob), (1- ((6*0.6)*(0.4)))/((5*0.6)*(0.4)))
  expect_length(aux_kurtosis(trials, prob), 1)
  expect_type(aux_kurtosis(trials, prob), "double")
})
