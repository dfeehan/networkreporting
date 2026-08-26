# Tests for network_survival_estimator(), the generic entry point.
#
# siblingsurvival::sibling_estimator() is this function with the sibling names
# and the clique tie filled in, so the load-bearing test here is that the two
# agree exactly. That is what makes it one implementation rather than two.

library(tibble)
library(dplyr)

# The 4-ego fixture from siblingsurvival's helper-simulate.R, restated here so
# this package's tests do not depend on that one. Expected rates, derived
# analytically there:
#   ASDR.agg = 36/353 per year
#   ASDR.ind = 4/29   per year
four_ego_dat <- function() {
  doi <- 600
  dod <- c(-1, -1, 570, -1, 550, 550, -1, -1)
  tibble(
    ego_id       = c(1, 1, 2, 2, 3, 3, 4, 4),
    alter_id     = c("A", "B", "C", "D", "E", "F", "G", "H"),
    dob          = 0,
    doi          = doi,
    dod          = dod,
    start_obs    = 0,
    end_obs      = ifelse(dod == -1, doi, dod + 1),
    in_frame     = c(1, 1, 0, 1, 0, 0, 1, 1),
    sex          = "f",
    weight       = 1)
}

nse_cell_config <- function() {
  cell_config(age.groups   = "5yr",
              time.periods = "5yr_beforeinterview",
              start.obs    = "start_obs",
              end.obs      = "end_obs",
              event        = "dod",
              age.offset   = "dob",
              time.offset  = "doi",
              exp.scale    = 1/12)
}

run_nse <- function(tie = tie_config("clique", name = "sibs"), ...) {
  network_survival_estimator(rel.dat         = four_ego_dat(),
                             ego.id          = "ego_id",
                             alter.id        = "alter_id",
                             frame.indicator = "in_frame",
                             alter.sex       = "sex",
                             cell.config     = nse_cell_config(),
                             weights         = "weight",
                             tie             = tie,
                             ...)
}

# ---------------------------------------------------------------------------
# The tie is required
# ---------------------------------------------------------------------------
test_that("the estimator refuses to run without a declared tie", {
  # no default, deliberately: applicability cannot be read off the data, and a
  # wrong clique default silently biases a rate rather than failing
  expect_error(
    network_survival_estimator(rel.dat         = four_ego_dat(),
                               ego.id          = "ego_id",
                               alter.id        = "alter_id",
                               frame.indicator = "in_frame",
                               alter.sex       = "sex",
                               cell.config     = nse_cell_config(),
                               weights         = "weight"),
    "needs a tie")
})

test_that("the no-tie message points at the sibling wrapper", {
  msg <- tryCatch(
    network_survival_estimator(rel.dat = four_ego_dat(), ego.id = "ego_id",
                               alter.id = "alter_id", frame.indicator = "in_frame",
                               alter.sex = "sex", cell.config = nse_cell_config(),
                               weights = "weight"),
    error = function(e) conditionMessage(e))
  expect_match(msg, "sibling_estimator")
})

test_that("the clique rule is still refused on a non-clique tie", {
  expect_error(run_nse(tie = tie_config("group", name = "cousins")),
               "only valid for tie structure")
})

# ---------------------------------------------------------------------------
# It computes what it should
# ---------------------------------------------------------------------------
test_that("the estimates match the analytically derived values", {
  res <- run_nse()

  agg <- res$asdr.agg %>% filter(alter.age == "[45,50)")
  ind <- res$asdr.ind %>% filter(alter.age == "[45,50)")

  expect_equal(agg$asdr.hat, 36/353)
  expect_equal(ind$asdr.hat, 4/29)
})

test_that("output columns are generic, not sibling-flavoured", {
  res <- run_nse()
  expect_true("alter.age" %in% names(res$asdr.ind))
  expect_false("sib.age"  %in% names(res$asdr.ind))
  # and the caller's own sex column name comes back
  expect_true("sex" %in% names(res$asdr.ind))
})

test_that("provenance comes back with the estimate", {
  res <- run_nse()
  expect_s3_class(res$vis_provenance, "vis_provenance")
  expect_equal(res$vis_provenance$rule, "clique")
  expect_equal(res$vis_provenance$tie, "clique")
})

# ---------------------------------------------------------------------------
# A rule's required columns survive the trip
# ---------------------------------------------------------------------------
test_that("a rule that needs an extra column gets it", {
  # the estimator joins a fixed set of alter covariates onto the reports; a rule
  # needing anything else -- vis_from_group_size() needs the group size column --
  # would otherwise find it missing, and be usable through
  # apply_visibility_rule() but not through the estimator
  d <- four_ego_dat() %>% mutate(n_group_and_F = c(4, 4, 3, 3, 5, 5, 4, 4))

  res <- network_survival_estimator(
    rel.dat = d, ego.id = "ego_id", alter.id = "alter_id",
    frame.indicator = "in_frame", alter.sex = "sex",
    cell.config = nse_cell_config(), weights = "weight",
    visibility = vis_from_group_size("n_group_and_F", label = "supplied"),
    tie = tie_config("group", name = "cousins"))

  expect_equal(res$vis_provenance$rule, "supplied")
  expect_true(is.finite(res$asdr.ind$asdr.hat[res$asdr.ind$alter.age == "[45,50)"]))
})

test_that("a non-clique tie can be estimated end to end", {
  # the whole point of extracting the generic: before it, there was no entry
  # point that would run a tie which is not a clique
  d <- four_ego_dat() %>% mutate(n_group_and_F = c(4, 4, 3, 3, 5, 5, 4, 4))

  res <- network_survival_estimator(
    rel.dat = d, ego.id = "ego_id", alter.id = "alter_id",
    frame.indicator = "in_frame", alter.sex = "sex",
    cell.config = nse_cell_config(), weights = "weight",
    visibility = vis_from_group_size("n_group_and_F"),
    tie = tie_config("group", name = "cousins"))

  expect_equal(res$vis_provenance$tie, "group")
  expect_gt(nrow(res$asdr.ind), 0)
})

# ---------------------------------------------------------------------------
# Column checks
# ---------------------------------------------------------------------------
test_that("a wrong column name is reported with the columns that do exist", {
  expect_error(
    network_survival_estimator(rel.dat = four_ego_dat(), ego.id = "ego_id",
                               alter.id = "no_such_col", frame.indicator = "in_frame",
                               alter.sex = "sex", cell.config = nse_cell_config(),
                               weights = "weight",
                               tie = tie_config("clique", name = "sibs")),
    "alter.id='no_such_col'")
})

test_that("a wrapper can have that message use its own argument names", {
  # sibling_estimator() passes .arg.labels so that someone who wrote sib.id= is
  # not told that alter.id is wrong -- an argument they never used
  msg <- tryCatch(
    network_survival_estimator(rel.dat = four_ego_dat(), ego.id = "ego_id",
                               alter.id = "no_such_col", frame.indicator = "in_frame",
                               alter.sex = "sex", cell.config = nse_cell_config(),
                               weights = "weight",
                               tie = tie_config("clique", name = "sibs"),
                               .arg.labels = c(alter.id = "sib.id"),
                               .data.label = "sib.dat"),
    error = function(e) conditionMessage(e))

  expect_match(msg, "sib.id='no_such_col'")
  expect_match(msg, "not found in sib.dat")
  expect_false(grepl("alter.id=", msg))
})

# ---------------------------------------------------------------------------
# Bootstrap
# ---------------------------------------------------------------------------
test_that("bootstrap weights produce intervals", {
  set.seed(1)
  boot <- tibble(ego_id = 1:4)
  for (m in 1:20) boot[[paste0("boot_weight_", m)]] <-
    stats::rpois(4, lambda = 1) + 0.5

  res <- suppressWarnings(run_nse(boot.weights = boot, return.boot = TRUE))

  row <- res$asdr.ind %>% filter(alter.age == "[45,50)")
  expect_true(is.finite(row$asdr.hat.ci.low))
  expect_true(is.finite(row$asdr.hat.ci.high))
  expect_lte(row$asdr.hat.ci.low, row$asdr.hat.ci.high)

  expect_true("alter.age" %in% names(res$boot.asdr.ind))
})
