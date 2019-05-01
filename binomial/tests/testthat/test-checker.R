context("Checker arguments")

test_that("check_prob works as expected",{
  expect_true(check_prob(0.2))
  expect_error(check_prob(1:5))
  expect_error(check_prob(c(1/2,1/3)))
  expect_error(check_prob("one"))
  expect_error(check_prob(FALSE))
})

test_that("check_trials works as expected",{
  expect_true(check_trials(4))
  expect_error(check_trials(1:5))
  expect_error(check_trials(-5))
  expect_error(check_trials(8.5))
})

test_that("check_success works as expected",{
  expect_true(check_success(1:3,5))
  expect_true(check_success(6,10))
  expect_error(check_success(4:8,5))
  expect_error(check_success(7,3))
  expect_error(check_success(7.5,10))
  expect_error(check_success(-5,10))
  expect_error(check_success(-3.5,9))
})
