##########################################################
## test_helpers.R
##
## unit tests for helper functions:
##   df.to.kpvec, add.kp, topcode.var, topcode.data,
##   estimate.error (helper_functions.r),
##   summation.estimator (summation.r),
##   rdsII.estimator (rds.r)
##########################################################

####################################
## df.to.kpvec
context("helpers - df.to.kpvec")

test_that("df.to.kpvec returns correctly named vector", {
  kp.df <- data.frame(known.popn = c("nurses", "teachers", "widowers"),
                      size       = c(50000, 120000, 80000),
                      stringsAsFactors = FALSE)

  result <- df.to.kpvec(kp.df, kp.var = "known.popn", kp.value = "size")

  expect_equal(length(result), 3)
  expect_equal(names(result), c("nurses", "teachers", "widowers"))
  expect_equal(as.numeric(result), c(50000, 120000, 80000))
})


####################################
## add.kp
context("helpers - add.kp")

test_that("add.kp attaches known.popns attribute", {
  df <- data.frame(x = 1:5)
  kp.vec <- c(nurses = 50000, teachers = 120000)

  result <- add.kp(df, kp.vec)

  expect_equal(attr(result, "known.popns"), kp.vec)
})

test_that("add.kp attaches total.popn.size when provided", {
  df <- data.frame(x = 1:5)
  kp.vec <- c(nurses = 50000)

  result <- add.kp(df, kp.vec, total.popn.size = 1e6)

  expect_equal(attr(result, "total.popn.size"), 1e6)
})

test_that("add.kp does not attach total.popn.size when not provided", {
  df <- data.frame(x = 1:5)
  kp.vec <- c(nurses = 50000)

  result <- add.kp(df, kp.vec)

  expect_null(attr(result, "total.popn.size"))
})


####################################
## topcode.var
context("helpers - topcode.var")

test_that("topcode.var replaces values above max with max", {
  x <- c(1, 5, 10, 20, 100)
  result <- topcode.var(x, max = 10)
  expect_equal(result, c(1, 5, 10, 10, 10))
})

test_that("topcode.var leaves values at or below max unchanged", {
  x <- c(1, 5, 10)
  result <- topcode.var(x, max = 10)
  expect_equal(result, c(1, 5, 10))
})

test_that("topcode.var converts to.na values to NA before topcoding", {
  x <- c(1, 5, 99, 10, 20)
  result <- topcode.var(x, max = 10, to.na = 99)
  expect_true(is.na(result[3]))
  expect_equal(result[1], 1)
  expect_equal(result[4], 10)
  expect_equal(result[5], 10)
})

test_that("topcode.var leaves ignored values unchanged even if above max", {
  x <- c(1, 5, 99, 10, 20)
  result <- topcode.var(x, max = 10, ignore = 99)
  expect_equal(result[3], 99)
  expect_equal(result[5], 10)
})

test_that("topcode.var stops on non-numeric input", {
  expect_error(topcode.var(c("a", "b"), max = 10))
})


####################################
## topcode.data
context("helpers - topcode.data")

test_that("topcode.data topcodes specified columns", {
  df <- data.frame(a = c(1, 5, 20),
                   b = c(3, 15, 7),
                   c = c(10, 10, 10))

  result <- topcode.data(df, vars = c("a", "b"), max = 10)

  expect_equal(result$a, c(1, 5, 10))
  expect_equal(result$b, c(3, 10, 7))
  expect_equal(result$c, c(10, 10, 10))  # untouched
})


####################################
## estimate.error
context("helpers - estimate.error")

test_that("estimate.error computes correct error metrics (positive error)", {
  result <- estimate.error(estimate = 5, truth = 4)
  expect_equal(as.numeric(result[, "err"]),    1)
  expect_equal(as.numeric(result[, "abserr"]), 1)
  expect_equal(as.numeric(result[, "sqerr"]),  1)
  expect_equal(as.numeric(result[, "relerr"]), 0.25)
})

test_that("estimate.error computes correct error metrics (negative error)", {
  result <- estimate.error(estimate = 3, truth = 4)
  expect_equal(as.numeric(result[, "err"]),    -1)
  expect_equal(as.numeric(result[, "abserr"]),  1)
  expect_equal(as.numeric(result[, "sqerr"]),   1)
  expect_equal(as.numeric(result[, "relerr"]), 0.25)
})

test_that("estimate.error handles zero error", {
  result <- estimate.error(estimate = 4, truth = 4)
  expect_equal(as.numeric(result[, "err"]),    0)
  expect_equal(as.numeric(result[, "abserr"]), 0)
  expect_equal(as.numeric(result[, "sqerr"]),  0)
  expect_equal(as.numeric(result[, "relerr"]), 0)
})


####################################
## summation.estimator
context("helpers - summation.estimator")

test_that("summation.estimator returns rowSums of specified columns", {
  df <- data.frame(A = c(1, 4),
                   B = c(2, 5),
                   C = c(3, 6))
  result <- summation.estimator(df, sum.q = c("A", "B", "C"))
  expect_equal(as.numeric(result), c(6, 15))
})

test_that("summation.estimator uses sum.q attribute when not passed directly", {
  df <- data.frame(A = c(1, 4), B = c(2, 5))
  attr(df, "sum.q") <- c("A", "B")
  result <- summation.estimator(df)
  expect_equal(as.numeric(result), c(3, 9))
})

test_that("summation.estimator stops when sum.q is missing and no attribute", {
  df <- data.frame(A = c(1, 4), B = c(2, 5))
  expect_error(summation.estimator(df))
})

test_that("summation.estimator stops for missing != 'ignore'", {
  df <- data.frame(A = c(1, 4), B = c(2, 5))
  expect_error(summation.estimator(df, sum.q = c("A", "B"), missing = "complete.obs"))
})


####################################
## rdsII.estimator
context("helpers - rdsII.estimator")

test_that("rdsII.estimator returns correct hand-computed value", {
  # d.hat = c(2, 4, 2), y = c(1, 0, 1)
  # num = c(0.5, 0, 0.5), denom = c(0.5, 0.25, 0.5)
  # estimate = sum(num)/sum(denom) = 1.0/1.25 = 0.8
  df <- data.frame(d = c(2, 4, 2), y = c(1, 0, 1))
  result <- rdsII.estimator(df, d.hat.vals = "d", y.vals = "y")
  expect_equal(result, 0.8)
})

test_that("rdsII.estimator with complete.obs excludes NA rows", {
  # Row 2 has NA in y; should only use rows 1 and 3
  # d = c(2, 4, 2), y = c(1, NA, 1)
  # Using rows 1 and 3: num = c(0.5, 0.5), denom = c(0.5, 0.5)
  # estimate = 1.0/1.0 = 1.0
  df <- data.frame(d = c(2, 4, 2), y = c(1, NA, 1))
  result <- rdsII.estimator(df, d.hat.vals = "d", y.vals = "y",
                             missing = "complete.obs")
  expect_equal(result, 1.0)
})

test_that("rdsII.estimator stops on invalid missing argument", {
  df <- data.frame(d = c(2, 4), y = c(1, 0))
  expect_error(rdsII.estimator(df, d.hat.vals = "d", y.vals = "y",
                                missing = "badoption"))
})
