context("binomial arguments")

# bin_choose

test_that("bin_choose works as expected", {
  n = 5
  k = 3
  expect_equal(bin_choose(n, k), 10)
  expect_length(bin_choose(n, k), 1)
  expect_type(bin_choose(n, k), "double")
})

# bin_probability

test_that("bin_probability works as expected", {
  n = 5
  k = 3
  p = 0.5
  expect_equal(bin_probability(k,n,p), bin_choose(5,3)*(p^k)*(1-p)^(n-k))
  expect_length(bin_probability(k,n,p), 1)
  expect_type(bin_probability(k,n,p), "double")
})

# bin_distribution

test_that("bin_distribution works as expected",{
  n = 5
  p = 0.5
  expect_equal(bin_distribution(n,p)$probability,bin_probability(0:5,5,0.5))
  expect_type(bin_distribution(n,p),"list")
  expect_equal(class(bin_distribution(n,p)),c("bindis","data.frame"))
})


# bin_cumulative
test_that("bin_cumulative works as expected",{
  n = 5
  p = 0.5
  expect_equal(bin_cumulative(n,p)$probability,bin_probability(0:5,5,0.5))
  expect_equal(bin_cumulative(n,p)$cumulative[6],1)
  expect_type(bin_cumulative(n,p),"list")
  expect_equal(class(bin_cumulative(n,p)),c("bincum","data.frame"))
})
