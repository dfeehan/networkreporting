# Tests for vis_from_model(): visibility predicted by a fitted model.
#
# This is the rule the fit/predict split was designed for, so a fair part of
# what is tested here is that the design held: non-integer visibility works end
# to end, is_estimated routes the bootstrap correctly, and no interface changed
# to accommodate it.

library(tibble)
library(dplyr)

# Donors whose group size really does depend on a continuous predictor, so a
# model has something to find and we know what the right answer looks like.
model_donors <- function(n = 400, seed = 7) {
  set.seed(seed)
  d <- tibble(.ego.id = seq_len(n),
              age     = runif(n, 20, 60),
              w       = 1)
  # E[S] = exp(a + b*age); y.F = S - 1
  d$y.F <- stats::rpois(n, lambda = exp(0.2 + 0.02 * d$age))
  d
}

model_alters <- function() {
  tibble(.ego.id   = 1:6,
         .sib.in.F = c(1, 0, 1, 0, 1, 0),
         age       = c(25, 25, 40, 40, 55, 55))
}

fit_predict <- function(rule, donors = model_donors(), alters = model_alters()) {
  rule$predict(alters, rule$fit(donors, "w"))
}

# ---------------------------------------------------------------------------
# The contract Phase 1 was built to accept
# ---------------------------------------------------------------------------
test_that("a model rule is estimated, and makes no structural assumption", {
  m <- vis_from_model(~ age)
  expect_true(m$is_estimated)          # so the bootstrap refits it
  expect_true(is.na(m$applies_to))     # so it needs no tie
})

test_that("it needs no tie declaration, and accepts any structure", {
  m <- vis_from_model(~ age, family = stats::poisson(link = "log"))
  d <- model_alters()

  expect_silent(apply_visibility_rule(m, d, ego.dat = model_donors(),
                                      weights = "w"))
  for (st in c("clique", "group", "star", "unbounded")) {
    got <- apply_visibility_rule(m, d, ego.dat = model_donors(), weights = "w",
                                 tie = tie_config(st, name = "t"))
    expect_true(all(is.finite(got$values$vis)))
  }
})

test_that("visibility is non-integer, and nothing downstream minds", {
  # the guard rail Phase 1 put in for exactly this rule
  got <- fit_predict(vis_from_model(~ age, family = stats::poisson(link = "log")))
  expect_true(all(got$vis != round(got$vis)))
  expect_equal(got$vis_weight, 1 / got$vis)
})

test_that("the frame split is preserved", {
  # an off-frame alter's visibility is exactly one more than an otherwise
  # identical on-frame alter's; that asymmetry is the only route into a rate
  got <- fit_predict(vis_from_model(~ age, family = stats::poisson(link = "log")))
  expect_equal(got$vis[2] - got$vis[1], 1)
  expect_equal(got$vis[4] - got$vis[3], 1)
  expect_equal(got$vis[6] - got$vis[5], 1)
})

# ---------------------------------------------------------------------------
# It recovers the truth when the truth is what it assumes
# ---------------------------------------------------------------------------
test_that("the model recovers the generating group size", {
  # donors are generated with E[S] = exp(0.2 + 0.02*age), so a log-link Poisson
  # fit should predict close to that. This is the test that the rule is fitting
  # the right response, not merely producing numbers.
  got <- fit_predict(vis_from_model(~ age, family = stats::poisson(link = "log")))

  # y.F ~ Poisson(exp(0.2 + 0.02*age)), and the modelled response is the GROUP
  # SIZE S = y.F + 1, so E[S] = exp(0.2 + 0.02*age) + 1. Visibility then takes
  # one off again for an on-frame alter.
  alters   <- model_alters()
  expected <- exp(0.2 + 0.02 * alters$age) + 1 - alters$.sib.in.F

  # generous tolerance: this is a finite sample, not an identity
  expect_equal(got$vis, expected, tolerance = 0.15)
})

test_that("a model beats a global mean when group size really varies", {
  # not a claim that models are better in general -- only that when the
  # generating process has structure, a rule that can express it does better
  donors <- model_donors()
  alters <- model_alters()
  truth  <- exp(0.2 + 0.02 * alters$age) + 1 - alters$.sib.in.F

  mod <- fit_predict(vis_from_model(~ age, family = stats::poisson(link = "log")),
                     donors, alters)
  gl  <- vis_from_donor(match_on = NULL, min_donors = 1)
  glo <- gl$predict(alters, gl$fit(donors, "w"))

  expect_lt(mean(abs(mod$vis - truth)), mean(abs(glo$vis - truth)))
})

# ---------------------------------------------------------------------------
# The formula
# ---------------------------------------------------------------------------
test_that("the formula must be one-sided", {
  expect_error(vis_from_model(.donor.S ~ age), "ONE-SIDED")
  expect_error(vis_from_model(), "one-sided formula")
  expect_error(vis_from_model("age"), "one-sided formula")
})

test_that("predictors map alter names onto donor names", {
  # the same asymmetry match_on handles: the formula is written in the alter's
  # vocabulary, and the donor frame may spell a covariate differently
  donors <- model_donors() %>% rename(ego_age = age)
  alters <- model_alters() %>% rename(.sib.age = age)

  m <- vis_from_model(~ .sib.age, predictors = c(.sib.age = "ego_age"),
                      family = stats::poisson(link = "log"))
  got <- m$predict(alters, m$fit(donors, "w"))

  expect_true(all(is.finite(got$vis)))
})

test_that("a predictor missing from the donor frame is reported clearly", {
  m <- vis_from_model(~ wealth)
  expect_error(m$fit(model_donors(), "w"), "missing the predictor column")
})

test_that("a predictor missing from the alter rows is reported clearly", {
  m  <- vis_from_model(~ age, family = stats::poisson(link = "log"))
  st <- m$fit(model_donors(), "w")
  expect_error(m$predict(tibble(.ego.id = 1, .sib.in.F = 0), st),
               "alter data is missing the predictor column")
})

test_that("donor data without y.F is refused", {
  m <- vis_from_model(~ age)
  expect_error(m$fit(tibble(.ego.id = 1:5, age = 30, w = 1), "w"), "y.F")
})

# ---------------------------------------------------------------------------
# Impossible predictions
# ---------------------------------------------------------------------------
test_that("a non-positive predicted group size is an error, not a weight", {
  # an identity link extrapolating below zero would otherwise turn into a
  # negative visibility and then a negative weight
  donors <- tibble(.ego.id = 1:60, age = seq(20, 60, length.out = 60), w = 1)
  donors$y.F <- pmax(0, round(8 - 0.2 * donors$age))   # falls to zero with age

  m  <- vis_from_model(~ age)                          # gaussian, identity link
  st <- m$fit(donors, "w")

  # extrapolate well beyond the donors
  far <- tibble(.ego.id = 1, .sib.in.F = 0, age = 200)
  expect_error(m$predict(far, st), "zero or negative")
})

test_that("that error names the fix", {
  donors <- tibble(.ego.id = 1:60, age = seq(20, 60, length.out = 60), w = 1)
  donors$y.F <- pmax(0, round(8 - 0.2 * donors$age))
  m   <- vis_from_model(~ age)
  msg <- tryCatch(m$predict(tibble(.ego.id = 1, .sib.in.F = 0, age = 200),
                            m$fit(donors, "w")),
                  error = function(e) conditionMessage(e))
  expect_match(msg, "log link")
})

# ---------------------------------------------------------------------------
# Provenance, and coalescing
# ---------------------------------------------------------------------------
test_that("provenance records that the model extrapolates beyond its donors", {
  m <- vis_from_model(~ age)
  expect_true(any(grepl("extrapolated to alters", m$assumptions)))
  expect_true(any(grepl("not that it is right", m$assumptions)))
})

test_that("the label names the model, or whatever the caller called it", {
  expect_equal(vis_from_model(~ age)$label, "model(age)")
  expect_equal(vis_from_model(~ age, label = "size model")$label, "size model")
})

test_that("it coalesces behind an exact rule", {
  # the intended shape for a mixed tie: derive where you can, model where you
  # cannot
  d <- tibble(.ego.id   = c(1, 2),
              .sib.in.F = c(0, 0),
              y.F       = c(3, NA),      # only ego 1 has a roster
              age       = c(30, 30))

  chain <- vis_coalesce(vis_from_clique(),
                        vis_from_model(~ age, family = stats::poisson(link = "log")))
  got <- apply_visibility_rule(chain, d, ego.dat = model_donors(), weights = "w",
                               tie = tie_config("clique", name = "t"))

  expect_equal(got$values$vis[1], 4)         # clique: y.F + 1
  expect_equal(got$values$vis_tier, c(1L, 2L))
  expect_true(is.finite(got$values$vis[2]))
})

test_that("a coalesced chain containing a model is estimated", {
  chain <- vis_coalesce(vis_from_clique(), vis_from_model(~ age))
  expect_true(chain$is_estimated)
})
