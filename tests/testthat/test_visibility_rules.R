# Unit tests for the visibility rule layer.
#
# Style follows siblingsurvival's test_sibling_estimator.R: build the data
# directly, derive the expected value analytically, and show the arithmetic in
# a comment so a reader can check the test rather than trust it.
#
# The one fact worth keeping in mind throughout: `vis` is a COUNT (how many
# frame members could have reported this alter) and `vis_weight` is its
# reciprocal. The estimator consumes the reciprocal, under the older name
# `ind_vis`.

library(tibble)

# ---------------------------------------------------------------------------
# helper: a small ego X alter frame with a known y.F per ego
# ---------------------------------------------------------------------------
# ego 1 reports 3 alters, 2 of them on frame  -> y.F = 2
# ego 2 reports 2 alters, 1 of them on frame  -> y.F = 1
make_alters <- function() {
  tibble(
    .ego.id   = c(1, 1, 1, 2, 2),
    .sib.id   = c("A", "B", "C", "D", "E"),
    .sib.in.F = c(1,   1,   0,   1,   0),
    .sib.sex  = c("f", "f", "m", "f", "m"),
    y.F       = c(2,   2,   2,   1,   1),
    sib.occ   = c(0,   0,   1,   0,   1),
    sib.exp   = c(10,  10,   4,  10,   6)
  )
}

apply_rule <- function(rule, dat) {
  rule$predict(dat, rule$fit(NULL, NULL))
}

# ---------------------------------------------------------------------------
# Test: the clique rule reproduces calculate_sib_ind_visibility() bit for bit
# ---------------------------------------------------------------------------
# The rule is: on frame -> vis = y.F ; off frame -> vis = y.F + 1
#
#   A: on  frame, y.F = 2 -> vis = 2, weight = 1/2
#   B: on  frame, y.F = 2 -> vis = 2, weight = 1/2
#   C: off frame, y.F = 2 -> vis = 3, weight = 1/3
#   D: on  frame, y.F = 1 -> vis = 1, weight = 1
#   E: off frame, y.F = 1 -> vis = 2, weight = 1/2
test_that("vis_from_clique reproduces the historical rule exactly", {
  dat <- make_alters()
  got <- apply_rule(vis_from_clique(), dat)

  expect_equal(got$vis,        c(2, 2, 3, 1, 2))
  expect_equal(got$vis_weight, c(1/2, 1/2, 1/3, 1, 1/2))
  expect_equal(unique(got$vis_rule), "clique")
})

test_that("vis_from_clique agrees with add_esc_ind_vis on the same data", {
  dat <- make_alters()

  # add_esc_ind_vis derives y.F itself from the alter roster, so give it one
  sib.dat <- dat %>% dplyr::select(.ego.id, .sib.id, .sib.in.F)

  via_helper <- add_esc_ind_vis(dat %>% dplyr::select(-y.F),
                                ego.id = ".ego.id",
                                sib.dat = sib.dat,
                                sib.frame.indicator = ".sib.in.F")
  via_rule <- apply_rule(vis_from_clique(), dat)

  expect_equal(via_helper$ind_vis, via_rule$vis_weight)
})

test_that("ego.in.group = FALSE drops the +1, and nothing else", {
  dat <- make_alters()
  got <- apply_rule(vis_from_clique(ego.in.group = FALSE), dat)

  # without ego in the group: on frame -> y.F - 1, off frame -> y.F
  expect_equal(got$vis, c(1, 1, 2, 0, 1))
})

test_that("vis_from_clique is not estimated, so the bootstrap freezes it", {
  expect_false(vis_from_clique()$is_estimated)
  expect_true(vis_from_donor()$is_estimated)
})

# ---------------------------------------------------------------------------
# Test: degenerate donor -- donor rule and clique rule agree exactly
# ---------------------------------------------------------------------------
# If every ego has the same y.F, the donor summary is that same number whatever
# the statistic, so borrowing is indistinguishable from deriving.
test_that("a donor drawn from egos who all share one y.F reproduces the clique rule", {
  dat <- tibble(
    .ego.id   = c(1, 1, 2, 2),
    .sib.in.F = c(1, 0, 1, 0),
    y.F       = c(3, 3, 3, 3)
  )
  egos <- tibble(.ego.id = c(1, 2), y.F = c(3, 3), w = c(1, 1))

  clique <- apply_rule(vis_from_clique(), dat)

  rule  <- vis_from_donor(match_on = NULL, min_donors = 1)
  donor <- rule$predict(dat, rule$fit(egos, "w"))

  expect_equal(donor$vis, clique$vis)
  expect_equal(donor$vis_weight, clique$vis_weight)
})

# ---------------------------------------------------------------------------
# Test: the on-frame / off-frame split is what keeps visibility in the rate
# ---------------------------------------------------------------------------
# A rule that gives every alter in a cell the same visibility v divides both
# numerator and denominator by v, so it cancels and reduces exactly to the
# aggregate estimator. This is executable documentation of that fact.
test_that("a visibility constant within a cell cancels out of the rate", {
  dat <- make_alters()

  # a deliberately flat rule: every alter gets the same visibility
  flat <- visibility_rule(
    label = "flat", requires = character(0), is_estimated = FALSE,
    fit = function(donor.dat, weights) list(),
    predict = function(alter.rows, state) {
      tibble(vis = rep(4, nrow(alter.rows)),
             vis_weight = rep(1/4, nrow(alter.rows)),
             vis_rule = "flat")
    })

  vw <- apply_rule(flat, dat)$vis_weight

  ind_rate <- sum(dat$sib.occ * vw) / sum(dat$sib.exp * vw)
  agg_rate <- sum(dat$sib.occ)      / sum(dat$sib.exp)

  expect_equal(ind_rate, agg_rate)

  # ... whereas the clique rule, which preserves the split, does NOT cancel
  cw <- apply_rule(vis_from_clique(), dat)$vis_weight
  clique_rate <- sum(dat$sib.occ * cw) / sum(dat$sib.exp * cw)
  expect_false(isTRUE(all.equal(clique_rate, agg_rate)))
})

# ---------------------------------------------------------------------------
# Test: the closed-form identity the cheap bootstrap path relies on
# ---------------------------------------------------------------------------
# With a group size S constant within a cell,
#
#   num   = y.DandFcell / (S-1) + y.DandnotFcell / S
#   denom = y.NandFcell / (S-1) + y.NandnotFcell / S
#
# must equal the per-alter sum. For mortality y.DandFcell is 0.
test_that("the frame-split identity equals the per-alter sum", {
  dat <- make_alters()
  S   <- 5  # group size, constant within the cell

  vis <- S - dat$.sib.in.F        # S-1 on frame, S off it
  per_alter_num   <- sum(dat$sib.occ / vis)
  per_alter_denom <- sum(dat$sib.exp / vis)

  y.DandFcell    <- sum(dat$.sib.in.F * dat$sib.occ)
  y.DandnotFcell <- sum(dat$sib.occ) - y.DandFcell
  y.NandFcell    <- sum(dat$.sib.in.F * dat$sib.exp)
  y.NandnotFcell <- sum(dat$sib.exp) - y.NandFcell

  identity_num   <- y.DandFcell / (S - 1) + y.DandnotFcell / S
  identity_denom <- y.NandFcell / (S - 1) + y.NandnotFcell / S

  expect_equal(identity_num,   per_alter_num)
  expect_equal(identity_denom, per_alter_denom)
})

# ---------------------------------------------------------------------------
# Test: harmonic <= arithmetic, by Jensen
# ---------------------------------------------------------------------------
test_that("the harmonic mean is at or below the arithmetic mean", {
  x <- c(1, 2, 3, 10)
  w <- c(1, 1, 1, 1)

  h <- vis_statistic(x, w, "harmonic")
  a <- vis_statistic(x, w, "arithmetic")

  expect_lt(h, a)

  # they coincide exactly when the values have no variance
  expect_equal(vis_statistic(rep(4, 5), rep(1, 5), "harmonic"),
               vis_statistic(rep(4, 5), rep(1, 5), "arithmetic"))

  # and the weighted harmonic mean is sum(w) / sum(w/x)
  expect_equal(whmean(c(2, 4), c(1, 3)), 4 / (1/2 + 3/4))
})

test_that("whmean refuses a non-positive visibility", {
  # a visibility of zero would say nobody could have reported an alter who was,
  # in fact, reported
  expect_error(whmean(c(1, 0, 2), c(1, 1, 1)), "strictly positive")
})

test_that("a donor rule prefers the harmonic mean by default", {
  expect_equal(vis_from_donor()$params$statistic, "harmonic")
})

# ---------------------------------------------------------------------------
# Test: donor coverage failure is loud, not silent
# ---------------------------------------------------------------------------
# match_on describes the ALTER; donors are respondents. A survey of women has
# no donor cell for a male alter. Today that would propagate NA into rates.
test_that("an alter cell with no donors errors under on_missing = 'error'", {
  alters <- tibble(.ego.id   = c(1, 2),
                   .sib.in.F = c(1, 0),
                   .sib.sex  = c("f", "m"))   # 'm' has no donor
  egos   <- tibble(.ego.id = 1:40, y.F = 3, sex = "f", w = 1)

  rule  <- vis_from_donor(match_on = c(.sib.sex = "sex"), min_donors = 5)
  state <- rule$fit(egos, "w")

  expect_error(rule$predict(alters, state), "no usable donor cell")
})

test_that("on_missing = 'fallback' uses the global value and records the tier", {
  alters <- tibble(.ego.id   = c(1, 2),
                   .sib.in.F = c(0, 0),
                   .sib.sex  = c("f", "m"))
  egos   <- tibble(.ego.id = 1:40, y.F = 3, sex = "f", w = 1)

  rule  <- vis_from_donor(match_on = c(.sib.sex = "sex"), min_donors = 5,
                          on_missing = "fallback")
  got   <- rule$predict(alters, rule$fit(egos, "w"))

  # every donor has y.F = 3, so S.hat = 4 globally and within the 'f' cell
  expect_equal(got$vis, c(4, 4))
  expect_true(grepl("fallback", got$vis_rule[2]))
  expect_false(grepl("fallback", got$vis_rule[1]))
})

test_that("min_donors blanks a cell that is too thin to trust", {
  alters <- tibble(.ego.id = 1, .sib.in.F = 0, .sib.sex = "f")
  egos   <- tibble(.ego.id = 1:3, y.F = 3, sex = "f", w = 1)   # only 3 donors

  rule <- vis_from_donor(match_on = c(.sib.sex = "sex"), min_donors = 25,
                         on_missing = "na")
  got  <- rule$predict(alters, rule$fit(egos, "w"))

  expect_true(is.na(got$vis))
})

test_that("a mis-specified match_on names the columns that are actually there", {
  egos <- tibble(.ego.id = 1:40, y.F = 3, sex = "f", w = 1)
  rule <- vis_from_donor(match_on = "wealth.quintile")
  expect_error(rule$fit(egos, "w"), "missing the column")
})

# ---------------------------------------------------------------------------
# Test: vis_coalesce falls through, and records which tier resolved each row
# ---------------------------------------------------------------------------
test_that("vis_coalesce takes the first rule that resolves each row", {
  # ego 1 has a y.F, so the clique rule can resolve its alter;
  # ego 2's y.F is missing, so it must fall through to the donor tier
  alters <- tibble(.ego.id   = c(1, 2),
                   .sib.in.F = c(0, 0),
                   .sib.sex  = c("f", "f"),
                   y.F       = c(2, NA))
  egos   <- tibble(.ego.id = 1:40, y.F = 3, sex = "f", w = 1)

  rule  <- vis_coalesce(vis_from_clique(),
                        vis_from_donor(match_on = NULL, min_donors = 1))
  got   <- rule$predict(alters, rule$fit(egos, "w"))

  expect_equal(got$vis, c(3, 4))          # clique: 2+1 ; donor: S.hat = 4
  expect_equal(got$vis_tier, c(1L, 2L))
  expect_equal(got$vis_rule[1], "clique")
})

test_that("vis_coalesce is estimated if any of its tiers is", {
  expect_false(vis_coalesce(vis_from_clique(), vis_from_clique())$is_estimated)
  expect_true(vis_coalesce(vis_from_clique(), vis_from_donor())$is_estimated)
})

test_that("vis_coalesce needs at least two rules", {
  expect_error(vis_coalesce(vis_from_clique()), "at least two")
})

# ---------------------------------------------------------------------------
# Test: provenance is complete
# ---------------------------------------------------------------------------
test_that("provenance tier counts sum to the number of alters", {
  dat <- make_alters()
  res <- apply_visibility_rule(vis_from_clique(), dat)

  expect_equal(sum(res$provenance$by_rule$n_alters), nrow(dat))
  expect_equal(res$provenance$n_alters, nrow(dat))
  expect_equal(res$provenance$n_unresolved, 0)
})

test_that("provenance reports the approximated share of deaths and of exposure separately", {
  # these are different numbers, and both matter: an approximation touching
  # little exposure but most deaths is not a small approximation
  dat <- make_alters()

  # a rule that resolves only the on-frame alters exactly and approximates the
  # rest; the off-frame alters carry both of the deaths
  half <- visibility_rule(
    label = "half", requires = ".sib.in.F", is_estimated = FALSE,
    fit = function(donor.dat, weights) list(),
    predict = function(alter.rows, state) {
      onf <- alter.rows$.sib.in.F == 1
      tibble(vis        = rep(3, nrow(alter.rows)),
             vis_weight = rep(1/3, nrow(alter.rows)),
             vis_rule   = ifelse(onf, "clique", "approx"))
    })

  res <- apply_visibility_rule(half, dat)
  p   <- res$provenance

  # deaths sit on rows C and E, both off frame, so 100% of deaths approximated
  expect_equal(p$share_deaths_approx, 1)
  # exposure is mostly on-frame, so a much smaller share
  expect_lt(p$share_exposure_approx, 0.5)
  expect_false(isTRUE(all.equal(p$share_deaths_approx, p$share_exposure_approx)))
})

test_that("provenance carries the rule's assumptions into output", {
  res <- apply_visibility_rule(vis_from_clique(), make_alters())
  expect_true(any(grepl("ego is a member of the group",
                        res$provenance$assumptions)))
})

# ---------------------------------------------------------------------------
# Test: non-integer visibility works end to end
# ---------------------------------------------------------------------------
# A donor mean is fractional, and a future model rule certainly will be, so
# nothing downstream may assume integrality. Worth an explicit test rather than
# relying on the donor tests to happen to cover it.
test_that("a fractional group size runs end to end and gives the expected answer", {
  # two donors with y.F 2 and 3 -> S of 3 and 4
  # harmonic mean of (3, 4) with equal weights = 2 / (1/3 + 1/4) = 24/7
  egos   <- tibble(.ego.id = c(1, 2), y.F = c(2, 3), w = c(1, 1))
  alters <- tibble(.ego.id = c(1, 2), .sib.in.F = c(1, 0))

  rule  <- vis_from_donor(match_on = NULL, min_donors = 1)
  got   <- rule$predict(alters, rule$fit(egos, "w"))

  S <- 24/7
  expect_equal(got$vis, c(S - 1, S))
  expect_false(any(got$vis == round(got$vis)))   # genuinely non-integer
  expect_equal(got$vis_weight, 1 / c(S - 1, S))
})

test_that("the arithmetic statistic reproduces the historical adjustment factor", {
  # the removed adj.factor was y.F.bar / (y.F.bar + 1), i.e. built from the
  # ARITHMETIC mean of y.F. With y.F of 2 and 4, y.F.bar = 3, so S.hat = 4.
  egos   <- tibble(.ego.id = c(1, 2), y.F = c(2, 4), w = c(1, 1))
  alters <- tibble(.ego.id = 1, .sib.in.F = 0)

  rule <- vis_from_donor(match_on = NULL, statistic = "arithmetic", min_donors = 1)
  got  <- rule$predict(alters, rule$fit(egos, "w"))

  y.F.bar <- 3
  expect_equal(got$vis, y.F.bar + 1)

  # and the harmonic default is strictly smaller, per Jensen
  hrule <- vis_from_donor(match_on = NULL, min_donors = 1)
  hgot  <- hrule$predict(alters, hrule$fit(egos, "w"))
  expect_lt(hgot$vis, got$vis)
})

# ---------------------------------------------------------------------------
# Test: apply_visibility_rule validates up front
# ---------------------------------------------------------------------------
test_that("a missing required column is reported with what is present", {
  dat <- make_alters() %>% dplyr::select(-y.F)
  expect_error(apply_visibility_rule(vis_from_clique(), dat),
               "needs a 'y.F' column")
})

test_that("apply_visibility_rule refuses something that is not a rule", {
  expect_error(apply_visibility_rule("clique", make_alters()),
               "must be a visibility_rule")
})

test_that("apply_visibility_rule derives y.F from sib.dat when asked", {
  dat     <- make_alters() %>% dplyr::select(-y.F)
  sib.dat <- make_alters() %>% dplyr::select(.ego.id, .sib.id, .sib.in.F)

  res <- apply_visibility_rule(vis_from_clique(), dat, sib.dat = sib.dat)

  expect_equal(res$data$y.F, c(2, 2, 2, 1, 1))
  expect_equal(res$values$vis, c(2, 2, 3, 1, 2))
})

# ---------------------------------------------------------------------------
# Test: get_group_info generalises get_sibship_info
# ---------------------------------------------------------------------------
test_that("get_group_info with ego.in.group = TRUE is get_sibship_info", {
  sib.dat <- make_alters() %>% dplyr::select(.ego.id, .sib.id, .sib.in.F)

  a <- get_sibship_info(sib.dat, ego.id = ".ego.id",
                        sib.frame.indicator = ".sib.in.F")
  b <- get_group_info(sib.dat, ego.id = ".ego.id",
                      frame.indicator = ".sib.in.F", ego.in.group = TRUE)

  expect_equal(a, b)
})

test_that("ego.in.group = FALSE removes ego from the group counts", {
  sib.dat <- make_alters() %>% dplyr::select(.ego.id, .sib.id, .sib.in.F)

  withego <- get_group_info(sib.dat, ".ego.id", ".sib.in.F", ego.in.group = TRUE)
  without <- get_group_info(sib.dat, ".ego.id", ".sib.in.F", ego.in.group = FALSE)

  # y.F counts alters only, so it is unaffected; the group totals lose one each
  expect_equal(withego$y.F, without$y.F)
  expect_equal(withego$yprime.F, without$yprime.F + 1)
  expect_equal(withego$sib.size, without$sib.size + 1)
})
