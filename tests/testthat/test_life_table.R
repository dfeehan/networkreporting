# Tests for nmx_to_nqx() and q15_to_50() (R/life_table.R).
#
# The defaults are chosen to match the two reference implementations, which both
# use a coefficient of 2.4 -- i.e. nax = 2.6 -- rather than the textbook 2.5:
#   DHS   Chap16_AM/AM_rates.do:1085   gen q5=5*mx/(1+2.4*mx)
#   MICS  TM.9 syntax                  compute fqx = 1 - (5*m/(1+2.4*m))

test_that("nmx_to_nqx reproduces the reference formula exactly", {
  nmx <- c(0.001, 0.005, 0.01)
  # what both reference implementations compute, written out longhand
  expect_equal(nmx_to_nqx(nmx), (5 * nmx) / (1 + 2.4 * nmx))
})

test_that("nax is an argument, and 2.5 gives the textbook denominator", {
  nmx <- c(0.001, 0.005, 0.01)
  expect_equal(nmx_to_nqx(nmx, nax = 2.5), (5 * nmx) / (1 + 2.5 * nmx))
  # and a larger nax means more of the interval is lived, hence a higher nqx
  expect_true(all(nmx_to_nqx(nmx, nax = 2.6) > nmx_to_nqx(nmx, nax = 2.5)))
})

test_that("nax must be less than the interval width", {
  expect_error(nmx_to_nqx(0.01, n = 5, nax = 5), "must be less than")
  expect_error(nmx_to_nqx(0.01, n = 5, nax = 7), "must be less than")
})

test_that("a zero rate gives a zero probability", {
  expect_equal(nmx_to_nqx(0), 0)
  expect_equal(q15_to_50(rep(0, 7)), 0)
})

# ---------------------------------------------------------------------------
# Validated against a published table
# ---------------------------------------------------------------------------
# The Gambia 2019-20 DHS (FR369) Table 14.1 gives age-specific rates and
# Table 14.2 gives 35q15 = 114 for women and 124 for men. Feeding the
# full-precision rates our own estimator produces through q15_to_50() returns
# 113.51 and 124.37, which round to the published integers. With nax = 2.5 the
# women's figure rounds to 113 instead, so the default is load-bearing.

test_that("q15_to_50 reproduces the published Gambia 2019-20 figures", {
  women <- c(0.9330, 1.5500, 2.2430, 3.2650, 3.5720, 6.2750, 6.2470) / 1000
  men   <- c(1.4400, 2.3000, 2.2900, 3.4000, 4.9100, 6.1100, 6.1100) / 1000

  expect_equal(round(q15_to_50(women)), 114)
  expect_equal(round(q15_to_50(men)),   124)
})

test_that("q15_to_50 needs exactly seven rates", {
  expect_error(q15_to_50(rep(0.001, 6)), "expected 7")
  expect_error(q15_to_50(rep(0.001, 8)), "expected 7")
})

test_that("q15_to_50 warns when handed rates per 1,000", {
  # the easy mistake: forgetting to divide by 1000 first
  expect_warning(q15_to_50(c(0.93, 1.55, 2.24, 3.27, 3.57, 6.27, 6.25)),
                 "per 1,000")
})

test_that("per scales the result", {
  nmx <- c(0.9330, 1.5500, 2.2430, 3.2650, 3.5720, 6.2750, 6.2470) / 1000
  expect_equal(q15_to_50(nmx, per = 1), q15_to_50(nmx, per = 1000) / 1000)
})
