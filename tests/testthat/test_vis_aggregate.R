# Tests for vis_aggregate(): visibility from aggregate relational data.
#
# This is the bridge between the package's two halves. The ARD / scale-up side
# estimates how many people a respondent knows; the estimator spine needs to
# know how many people could have reported an alter. Those are different
# quantities, and the conversion between them is an assumption the caller has to
# supply -- which is what most of these tests are about.

library(tibble)

ard_donors <- function(n = 200, degree = 250, seed = 3) {
  set.seed(seed)
  tibble(.ego.id = seq_len(n),
         d.hat   = rep(degree, n),   # constant, so the arithmetic is checkable
         sex     = rep(c("f", "m"), length.out = n),
         w       = 1)
}

ard_alters <- function() {
  tibble(.ego.id   = 1:4,
         .sib.in.F = c(1, 0, 1, 0),
         .sib.sex  = c("f", "f", "m", "m"))
}

fit_ard <- function(rule, donors = ard_donors(), alters = ard_alters()) {
  rule$predict(alters, rule$fit(donors, "w"))
}

# ---------------------------------------------------------------------------
# The assumption with no default
# ---------------------------------------------------------------------------
test_that("frame.ratio is required, and the message says why", {
  expect_error(vis_aggregate("d.hat"), "no default")

  msg <- tryCatch(vis_aggregate("d.hat"), error = function(e) conditionMessage(e))
  # the point is the mismatch between what ARD measures and what visibility is
  expect_match(msg, "WHOLE population")
  expect_match(msg, "FRAME-POPULATION")
  expect_match(msg, "scaled by a constant")
})

test_that("frame.ratio must be a share", {
  expect_error(vis_aggregate("d.hat", frame.ratio = 0), "in \\(0, 1\\]")
  expect_error(vis_aggregate("d.hat", frame.ratio = 1.5), "in \\(0, 1\\]")
  expect_error(vis_aggregate("d.hat", frame.ratio = c(0.1, 0.2)), "in \\(0, 1\\]")
})

test_that("a frame-only degree needs no conversion", {
  # if the caller already has connections-to-frame-members, there is nothing to
  # assume and nothing to supply
  r   <- vis_aggregate("d.hat", degree.counts = "frame")
  got <- fit_ard(r, ard_donors(degree = 6))
  expect_equal(got$vis, c(5, 6, 5, 6))
})

# ---------------------------------------------------------------------------
# The arithmetic
# ---------------------------------------------------------------------------
test_that("visibility is the degree scaled by the frame share", {
  # every donor knows 250 people; a fiftieth of the population is in the frame,
  # so an alter is reportable by about 5 frame members
  got <- fit_ard(vis_aggregate("d.hat", frame.ratio = 0.02))
  expect_equal(got$vis, c(4, 5, 4, 5))
})

test_that("the on-frame / off-frame split is preserved", {
  # without it every alter in a cell gets the same number, that number divides
  # out of the rate, and the ARD does no work at all
  got <- fit_ard(vis_aggregate("d.hat", frame.ratio = 0.02))
  expect_equal(got$vis[2] - got$vis[1], 1)
  expect_equal(got$vis[4] - got$vis[3], 1)
})

test_that("a flat visibility would reproduce the aggregate estimator", {
  # executable form of the reason the split matters: dividing occurrences and
  # exposure by the same constant leaves the ratio alone
  occ <- c(0, 1, 0, 1); exp <- c(10, 4, 10, 6)
  flat <- rep(1/5, 4)
  expect_equal(sum(occ * flat) / sum(exp * flat), sum(occ) / sum(exp))
})

test_that("matching on a covariate is supported", {
  donors <- ard_donors()
  donors$d.hat <- ifelse(donors$sex == "f", 200, 300)

  r   <- vis_aggregate("d.hat", frame.ratio = 0.02,
                       match_on = c(.sib.sex = "sex"))
  got <- fit_ard(r, donors)

  # f alters: 200 * .02 = 4 ; m alters: 300 * .02 = 6
  expect_equal(got$vis, c(3, 4, 5, 6))
})

# ---------------------------------------------------------------------------
# What it does and does not assume
# ---------------------------------------------------------------------------
test_that("it is estimated, so the bootstrap refits it", {
  expect_true(vis_aggregate("d.hat", frame.ratio = 0.02)$is_estimated)
})

test_that("it makes no structural assumption and needs no tie", {
  # the appeal of ARD: it does not care how the tie is shaped, so it works
  # where there is no roster to derive from
  r <- vis_aggregate("d.hat", frame.ratio = 0.02)
  expect_true(is.na(r$applies_to))

  d <- ard_alters()
  for (st in c("clique", "group", "star", "unbounded")) {
    got <- apply_visibility_rule(r, d, ego.dat = ard_donors(), weights = "w",
                                 tie = tie_config(st, name = "t"))
    expect_true(all(is.finite(got$values$vis)))
  }
})

test_that("the assumptions name the ones the method carries, not just the arithmetic", {
  a <- vis_aggregate("d.hat", frame.ratio = 0.02)$assumptions
  expect_true(any(grepl("roughly symmetric", a)))
  expect_true(any(grepl("stand in for alters", a)))
  expect_true(any(grepl("include the dead", a)))
  expect_true(any(grepl("frame.ratio = 0.02", a)))
})

# ---------------------------------------------------------------------------
# Impossible results
# ---------------------------------------------------------------------------
test_that("a frame share too small for the degrees is an error", {
  # a visibility at or below zero says nobody could have reported an alter who
  # was, in fact, reported
  r <- vis_aggregate("d.hat", frame.ratio = 0.001)
  expect_error(fit_ard(r), "at or below zero")
})

test_that("that message points at frame.ratio", {
  r <- vis_aggregate("d.hat", frame.ratio = 0.001)
  msg <- tryCatch(fit_ard(r), error = function(e) conditionMessage(e))
  expect_match(msg, "frame.ratio is too small")
})

test_that("donor data without the degree column is reported clearly", {
  r <- vis_aggregate("no_such_col", frame.ratio = 0.02)
  expect_error(r$fit(ard_donors(), "w"), "no 'no_such_col' column")
})

test_that("donors with no usable degree at all are refused", {
  r <- vis_aggregate("d.hat", frame.ratio = 0.02)
  expect_error(r$fit(ard_donors(degree = NA), "w"), "no donor has a usable degree")
})

test_that("degree.var is required", {
  expect_error(vis_aggregate(), "needs degree.var")
  expect_error(vis_aggregate(c("a", "b"), frame.ratio = 0.5), "needs degree.var")
})

# ---------------------------------------------------------------------------
# Working with the rest of the layer
# ---------------------------------------------------------------------------
test_that("it coalesces behind an exact rule", {
  # derive where a roster exists, fall back to ARD where it does not
  d <- tibble(.ego.id = c(1, 2), .sib.in.F = c(0, 0), y.F = c(3, NA))

  chain <- vis_coalesce(vis_from_clique(),
                        vis_aggregate("d.hat", frame.ratio = 0.02))
  got <- apply_visibility_rule(chain, d, ego.dat = ard_donors(), weights = "w",
                               tie = tie_config("clique", name = "t"))

  expect_equal(got$values$vis[1], 4)          # clique: y.F + 1
  expect_equal(got$values$vis_tier, c(1L, 2L))
  expect_equal(got$values$vis[2], 5)          # ard: 250 * 0.02
})

test_that("the label names the degree column, or whatever the caller called it", {
  expect_equal(vis_aggregate("d.hat", frame.ratio = 0.02)$label, "ard(d.hat)")
  expect_equal(vis_aggregate("d.hat", frame.ratio = 0.02, label = "scale-up")$label,
               "scale-up")
})
