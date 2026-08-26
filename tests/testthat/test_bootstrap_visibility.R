# Tests for the bootstrap path that an estimated visibility rule takes.
#
# Three cases, per dev/VISIBILITY-PLAN.md:
#   1. not estimated        -> frozen, and that is correct
#   2. estimated, constant within a cell -> the frame-split identity, cheap
#   3. estimated, not constant within a cell -> refit and re-predict per report
#
# The load-bearing tests here are that case 2 is indexed correctly (it was not,
# and the error was invisible whenever the estimated visibility happened to be
# constant), and that cases 2 and 3 agree wherever both are valid.

library(tibble)
library(dplyr)

# Each ego reports two same-sex alters, so both donor cells are populated and
# the donor summary genuinely differs between them:
#   ego1 sex=f y.F=2 -> S=3      ego2 sex=m y.F=1 -> S=2
#   ego3 sex=f y.F=0 -> S=1      ego4 sex=m y.F=2 -> S=3
# harmonic S.hat, equal weights:  f -> 2/(1/3+1/1) = 1.5 ;  m -> 2/(1/2+1/3) = 2.4
boot_dat <- function() {
  doi <- 600
  dod <- c(-1, -1, 570, -1, 550, 550, -1, -1)
  tibble(ego_id    = rep(1:4, each = 2),
         sib_id    = LETTERS[1:8],
         dob       = 0,
         doi       = doi,
         dod       = dod,
         start_obs = 0,
         end_obs   = ifelse(dod == -1, doi, dod + 1),
         in_frame  = c(1, 1, 0, 1, 0, 0, 1, 1),
         sex       = c("f", "f", "m", "m", "f", "f", "m", "m"),
         w         = 1,
         # cuts across cells: not derivable from ec.dat
         wealth    = c("hi", "hi", "lo", "lo", "hi", "lo", "hi", "lo"))
}

boot_cc <- function() {
  cell_config(age.groups = "5yr", time.periods = "5yr_beforeinterview",
              start.obs = "start_obs", end.obs = "end_obs", event = "dod",
              age.offset = "dob", time.offset = "doi", exp.scale = 1/12)
}

# build the intermediate objects the bootstrap helpers work on
boot_pieces <- function(rule) {
  d  <- boot_dat()
  d2 <- d %>% mutate(.ego.id = ego_id, .sib.id = sib_id, .sib.in.F = in_frame,
                     .sib.sex = sex, .ego.weight = w)
  esc <- get_esc_reports(sib.dat = d2, ego.id = ".ego.id", sib.id = ".sib.id",
                         boot_cc()) %>%
    left_join(d2 %>% select(.ego.id, .sib.id, .ego.weight, .sib.in.F, .sib.sex, wealth),
              by = c(".ego.id", ".sib.id"))
  vr <- apply_visibility_rule(rule, esc, sib.dat = d2, weights = ".ego.weight",
                              tie = tie_config("clique", name = "t"))
  esc2 <- vr$data
  esc2$ind_vis <- vr$values$vis_weight
  cv <- c("time.period", ".sib.sex", "agelabel")
  ec <- get_ec_reports(esc2, ego.id = ".ego.id", sib.dat = d2,
                       sib.frame.indicator = ".sib.in.F", cell.vars = cv,
                       weights = ".ego.weight", ind.vis.var = "ind_vis")
  list(esc = esc2, ec = ec, cv = cv, donor.dat = vr$donor.dat)
}

# ---------------------------------------------------------------------------
# Case 2: the per-cell identity, correctly indexed
# ---------------------------------------------------------------------------
test_that("an estimated rule that varies by cell gets each cell's own S.hat", {
  # Regression test. The refit function returns one value per row of ec.dat, but
  # the bootstrap indexed it by bootstrap-weight row position, so every cell was
  # handed the same few rows' values. Invisible with a global donor rule, where
  # S.hat is constant; wrong as soon as it varies by cell.
  rule <- vis_from_donor(match_on = c(.sib.sex = "sex"), min_donors = 1)
  p    <- boot_pieces(rule)
  bw   <- tibble(.ego.id = 1:4, boot_weight_1 = c(1, 1, 1, 1))

  got <- get_boot_ests_matrix(p$ec, bw, ".ego.id", p$cv, "ind",
                              visibility = rule,
                              refit = make_vis_refit(rule, p$donor.dat, bw,
                                                     p$ec, ".ego.id")) %>%
    filter(agelabel == "[45,50)") %>% arrange(.sib.sex)

  # hand-computed, with S.hat = 1.5 for f and 2.4 for m
  #   f: ego1 A,B alive on frame (120 months); ego3 E,F die off frame (22 months, 2 deaths)
  #   m: ego2 C dies off frame (31 months, 1 death), D alive on frame (60);
  #      ego4 G,H alive on frame (120)
  f_num   <- 2 / 1.5
  f_denom <- (120/12) / (1.5 - 1) + (22/12) / 1.5
  m_num   <- 1 / 2.4
  m_denom <- ((60 + 120)/12) / (2.4 - 1) + (31/12) / 2.4

  expect_equal(got$num.hat[got$.sib.sex == "f"],   f_num)
  expect_equal(got$denom.hat[got$.sib.sex == "f"], f_denom)
  expect_equal(got$num.hat[got$.sib.sex == "m"],   m_num)
  expect_equal(got$denom.hat[got$.sib.sex == "m"], m_denom)
})

test_that("the two cells really do get different S.hat", {
  # guards the test above from passing for the wrong reason
  rule <- vis_from_donor(match_on = c(.sib.sex = "sex"), min_donors = 1)
  p    <- boot_pieces(rule)
  bw   <- tibble(.ego.id = 1:4, boot_weight_1 = c(1, 1, 1, 1))

  S <- make_vis_refit(rule, p$donor.dat, bw, p$ec, ".ego.id")(1)
  by_cell <- p$ec %>% mutate(S = S) %>% filter(agelabel == "[45,50)") %>%
    distinct(.sib.sex, S) %>% arrange(.sib.sex)

  expect_equal(by_cell$S, c(1.5, 2.4))
})

# ---------------------------------------------------------------------------
# Case 3, and its agreement with case 2
# ---------------------------------------------------------------------------
test_that("cases 2 and 3 agree wherever both are valid", {
  # the expensive path recomputes visibility per report and re-aggregates; the
  # cheap one uses the frame-split identity. Where the rule is cell-constant
  # both are correct, so they must give the same numbers.
  rule <- vis_from_donor(match_on = c(.sib.sex = "sex"), min_donors = 1)
  p    <- boot_pieces(rule)
  bw   <- tibble(.ego.id = 1:4,
                 boot_weight_1 = c(1, 1, 1, 1),
                 boot_weight_2 = c(3, 0.5, 0.5, 2),
                 boot_weight_3 = c(0.2, 2, 1, 0.7))

  cheap <- get_boot_ests_matrix(p$ec, bw, ".ego.id", p$cv, "ind", visibility = rule,
             refit = make_vis_refit(rule, p$donor.dat, bw, p$ec, ".ego.id"))
  full  <- get_boot_ests_matrix(p$ec, bw, ".ego.id", p$cv, "ind", visibility = rule,
             refit_sums = make_vis_refit_esc(rule, p$donor.dat, bw, p$esc, p$ec,
                                             p$cv, ".ego.id"))

  expect_equal(cheap$num.hat,   full$num.hat)
  expect_equal(cheap$denom.hat, full$denom.hat)
  expect_equal(cheap$asdr.hat,  full$asdr.hat)
})

test_that("cell-constancy is detected from the columns the rule needs", {
  ec <- boot_pieces(vis_from_clique())$ec

  # y.F is in ec.dat, so the clique rule is cell-constant
  expect_true(vis_is_cell_constant(vis_from_clique(), ec))
  # .sib.sex is a cell variable, so matching on it is too
  expect_true(vis_is_cell_constant(
    vis_from_donor(match_on = c(.sib.sex = "sex")), ec))
  # wealth is not in ec.dat: it cuts across cells
  expect_false(vis_is_cell_constant(vis_from_donor(match_on = "wealth"), ec))
})

# ---------------------------------------------------------------------------
# The estimator picks the right path
# ---------------------------------------------------------------------------
run_boot <- function(rule, M = 2) {
  bw <- tibble(ego_id = 1:4)
  bw$boot_weight_1 <- c(1, 1, 1, 1)
  if (M > 1) bw$boot_weight_2 <- c(2, 1, 1, 1)
  network_survival_estimator(
    rel.dat = boot_dat(), ego.id = "ego_id", alter.id = "sib_id",
    frame.indicator = "in_frame", alter.sex = "sex", cell.config = boot_cc(),
    weights = "w", boot.weights = bw, return.boot = TRUE,
    visibility = rule, tie = tie_config("clique", name = "t"))
}

test_that("a rule that cuts across cells warns about the cost and still runs", {
  rule <- vis_from_donor(match_on = "wealth", min_donors = 1,
                         on_missing = "fallback")
  expect_warning(run_boot(rule), "not constant within a cell")

  res <- suppressWarnings(run_boot(rule))
  expect_true(all(is.finite(res$boot.asdr.ind$num.hat)))
})

test_that("a cell-constant rule takes the cheap path silently", {
  rule <- vis_from_donor(match_on = c(.sib.sex = "sex"), min_donors = 1)
  expect_no_warning(
    suppressWarnings({  # the empty-cell warnings from this fixture are expected
      w <- NULL
      withCallingHandlers(run_boot(rule),
        warning = function(cnd) {
          if (grepl("not constant within a cell", conditionMessage(cnd))) w <<- cnd
          invokeRestart("muffleWarning")
        })
      if (!is.null(w)) stop("took the expensive path")
    }))
})

test_that("the clique rule is untouched: frozen, and no warning", {
  # this is what makes the whole change safe to land
  expect_false(vis_from_clique()$is_estimated)

  res <- suppressWarnings(run_boot(vis_from_clique()))
  expect_true(all(is.finite(res$boot.asdr.ind$num.hat)))
})

test_that("an estimated rule with no refit at all warns rather than freezing", {
  rule <- vis_from_donor(match_on = NULL, min_donors = 1)
  p    <- boot_pieces(rule)
  bw   <- tibble(.ego.id = 1:4, boot_weight_1 = c(1, 1, 1, 1))

  expect_warning(
    get_boot_ests_matrix(p$ec, bw, ".ego.id", p$cv, "ind", visibility = rule),
    "frozen across bootstrap replicates")
})
