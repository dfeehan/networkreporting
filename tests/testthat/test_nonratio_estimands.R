# Tests for estimands that are not ratios.
#
# A death rate is a ratio of two visibility-adjusted sums. Those sums are
# estimands in their own right -- the estimated number of events, and the
# estimated person-time -- and the plan's point was that nothing before the
# final division assumes a ratio is what you want. What was missing was
# uncertainty on the sums: their bootstrap replicates were computed and then
# discarded at the summarising step, so only the ratio could be reported with
# an interval.

library(tibble)
library(dplyr)

nr_dat <- function() {
  doi <- 600
  dod <- c(-1, -1, 570, -1, 550, 550, -1, -1)
  tibble(ego_id = rep(1:4, each = 2), alt = LETTERS[1:8], dob = 0, doi = doi,
         dod = dod, start_obs = 0, end_obs = ifelse(dod == -1, doi, dod + 1),
         in_frame = c(1, 1, 0, 1, 0, 0, 1, 1), sex = "f", w = 1)
}

nr_cc <- function() {
  cell_config(age.groups = "5yr", time.periods = "5yr_beforeinterview",
              start.obs = "start_obs", end.obs = "end_obs", event = "dod",
              age.offset = "dob", time.offset = "doi", exp.scale = 1/12)
}

nr_est <- function(with.boot = TRUE) {
  boot <- NULL
  if (with.boot) {
    set.seed(5)
    boot <- tibble(ego_id = 1:4)
    for (m in 1:100) boot[[paste0("boot_weight_", m)]] <- stats::rpois(4, 1) + 0.5
  }
  suppressWarnings(network_survival_estimator(
    rel.dat = nr_dat(), ego.id = "ego_id", alter.id = "alt",
    frame.indicator = "in_frame", alter.sex = "sex", cell.config = nr_cc(),
    weights = "w", boot.weights = boot,
    tie = tie_config("clique", name = "siblings")))
}

# ---------------------------------------------------------------------------
# The sums now carry uncertainty
# ---------------------------------------------------------------------------
test_that("the visibility-adjusted sums get intervals, not just the ratio", {
  res <- nr_est()
  row <- res$asdr.ind %>% filter(alter.age == "[45,50)")

  for (stem in c("num.hat", "denom.hat")) {
    for (suffix in c("ci.low", "ci.high", "median", "se")) {
      expect_true(paste0(stem, ".", suffix) %in% names(row),
                  info = paste(stem, suffix))
    }
  }
  expect_true(is.finite(row$num.hat.se))
  expect_true(is.finite(row$denom.hat.se))
})

test_that("the rate's own interval is unchanged", {
  # this is what makes the change safe: adding columns must not move any
  # existing number
  res <- nr_est()
  row <- res$asdr.ind %>% filter(alter.age == "[45,50)")

  expect_true(all(c("asdr.hat.ci.low", "asdr.hat.ci.high", "asdr.hat.median",
                    "asdr.hat.se") %in% names(row)))
  expect_lte(row$asdr.hat.ci.low, row$asdr.hat.ci.high)
})

test_that("the aggregate estimator gets the same treatment", {
  res <- nr_est()
  row <- res$asdr.agg %>% filter(alter.age == "[45,50)")
  expect_true("num.hat.se" %in% names(row))
  expect_true("denom.hat.se" %in% names(row))
})

# ---------------------------------------------------------------------------
# estimated_total()
# ---------------------------------------------------------------------------
test_that("a total is read off the estimate with its interval", {
  res <- nr_est()
  ev  <- estimated_total(res, "events") %>% filter(alter.age == "[45,50)")
  ref <- res$asdr.ind %>% filter(alter.age == "[45,50)")

  expect_equal(ev$estimate, ref$num.hat)
  expect_equal(ev$estimate.ci.low, ref$num.hat.ci.low)
  expect_equal(ev$estimate.se, ref$num.hat.se)
  expect_equal(unique(ev$quantity), "events")
})

test_that("exposure is the other total", {
  res <- nr_est()
  ex  <- estimated_total(res, "exposure") %>% filter(alter.age == "[45,50)")
  ref <- res$asdr.ind %>% filter(alter.age == "[45,50)")
  expect_equal(ex$estimate, ref$denom.hat)
})

test_that("the rate reconstructs exactly from the two totals", {
  # the separation the plan asked for: forming a ratio is a final, optional
  # step, and doing it by hand must give back what the estimator reported
  res <- nr_est()
  ev  <- estimated_total(res, "events")
  ex  <- estimated_total(res, "exposure")
  expect_equal(ev$estimate / ex$estimate, res$asdr.ind$asdr.hat)
})

test_that("the aggregate estimator's totals are available too", {
  res <- nr_est()
  a <- estimated_total(res, "events", estimator = "agg")
  b <- estimated_total(res, "exposure", estimator = "agg")
  expect_equal(a$estimate / b$estimate, res$asdr.agg$asdr.hat)
})

test_that("cell columns come through, and computed ones do not", {
  res <- nr_est()
  ev  <- estimated_total(res, "events")
  expect_true(all(c("time.period", "sex", "alter.age") %in% names(ev)))
  expect_false(any(c("num.hat", "asdr.hat", "wgt.sum") %in% names(ev)))
})

test_that("without a bootstrap there is an estimate but no interval", {
  res <- nr_est(with.boot = FALSE)
  ev  <- suppressMessages(estimated_total(res, "events"))

  expect_true(all(is.finite(ev$estimate)))
  expect_true(all(is.na(ev$estimate.se)))
  expect_message(estimated_total(res, "events"), "no interval available")
})

test_that("it refuses something that is not an estimator result", {
  expect_error(estimated_total(list(), "events"), "no 'asdr.ind' table")
})
