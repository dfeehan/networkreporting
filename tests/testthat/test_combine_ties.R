# Tests for combining estimates across ties.
#
# Three operations, and the point of the tests is partly that they are NOT
# variations of one thing:
#   compare  lines results up, assumes nothing
#   pool     averages results, and has to reckon with the ties not being
#            independent, since the same respondents report all of them
#   union    combines DATA, not results, and needs an alter identity the
#            package does not carry

library(tibble)
library(dplyr)

# Two ties reported by the SAME eight respondents. That shared-respondent
# structure is what makes their estimates correlated, and what pool_ties() has
# to handle rather than assume away.
tie_dat <- function(seed, frame) {
  set.seed(seed)
  doi <- 600
  dod <- ifelse(runif(16) < 0.2, 560, -1)
  tibble(ego_id    = rep(1:8, each = 2),
         alt       = paste0("a", seed, "_", 1:16),
         dob       = 0,
         doi       = doi,
         dod       = dod,
         start_obs = 0,
         end_obs   = ifelse(dod == -1, doi, dod + 1),
         in_frame  = frame,
         sex       = "f",
         w         = 1)
}

tie_cc <- function() {
  cell_config(age.groups = "5yr", time.periods = "5yr_beforeinterview",
              start.obs = "start_obs", end.obs = "end_obs", event = "dod",
              age.offset = "dob", time.offset = "doi", exp.scale = 1/12)
}

# the SAME replicate weights for every tie: that is what carries the correlation
tie_boot <- function(M = 80) {
  set.seed(99)
  b <- tibble(ego_id = 1:8)
  for (m in seq_len(M)) b[[paste0("boot_weight_", m)]] <- stats::rpois(8, 1) + 0.5
  b
}

tie_est <- function(d, name, boot = tie_boot(), return.boot = TRUE) {
  suppressWarnings(network_survival_estimator(
    rel.dat = d, ego.id = "ego_id", alter.id = "alt",
    frame.indicator = "in_frame", alter.sex = "sex",
    cell.config = tie_cc(), weights = "w",
    boot.weights = boot, return.boot = return.boot,
    tie = tie_config("clique", name = name)))
}

two_ties <- function() {
  list(siblings  = tie_est(tie_dat(1, c(1,1,0,1,0,0,1,1,1,0,1,1,0,1,1,0)), "siblings"),
       household = tie_est(tie_dat(2, c(1,0,1,1,0,1,0,1,1,1,0,0,1,1,0,1)), "household"))
}

# ---------------------------------------------------------------------------
# compare
# ---------------------------------------------------------------------------
test_that("compare_ties lines the ties up on shared cells", {
  t <- two_ties()
  cmp <- compare_ties(siblings = t$siblings, household = t$household)

  expect_s3_class(cmp, "tie_comparison")
  expect_setequal(unique(cmp$tie), c("siblings", "household"))
  expect_true(all(c("time.period", "sex", "alter.age") %in%
                    attr(cmp, "cell.vars")))

  # one row per cell per tie
  expect_equal(nrow(cmp), nrow(t$siblings$asdr.ind) + nrow(t$household$asdr.ind))
})

test_that("compare_ties changes no estimate", {
  # it lines results up; it does not recompute anything
  t   <- two_ties()
  cmp <- compare_ties(siblings = t$siblings, household = t$household)

  sib <- cmp %>% filter(tie == "siblings", alter.age == "[45,50)")
  ref <- t$siblings$asdr.ind %>% filter(alter.age == "[45,50)")
  expect_equal(sib$asdr.hat, ref$asdr.hat)
  expect_equal(sib$asdr.hat.se, ref$asdr.hat.se)
})

test_that("compare_ties carries each tie's visibility provenance", {
  t   <- two_ties()
  cmp <- compare_ties(siblings = t$siblings, household = t$household)
  prov <- attr(cmp, "provenance")

  expect_setequal(names(prov), c("siblings", "household"))
  expect_equal(prov$siblings$rule, "clique")
  expect_equal(prov$household$tie_name, "household")
})

test_that("ties must be named", {
  t <- two_ties()
  expect_error(compare_ties(t$siblings, t$household), "must be named")
  expect_error(compare_ties(), "no ties given")
})

test_that("ties with no shared cells are refused, naming what each has", {
  t <- two_ties()
  odd <- t$household
  odd$asdr.ind <- odd$asdr.ind %>%
    rename(period = time.period, s = sex, ag = alter.age)

  expect_error(compare_ties(siblings = t$siblings, household = odd),
               "share no cell columns")
})

# ---------------------------------------------------------------------------
# pool
# ---------------------------------------------------------------------------
test_that("pool_ties defaults to pooling within replicates", {
  t <- two_ties()
  p <- pool_ties(siblings = t$siblings, household = t$household)

  expect_s3_class(p, "tie_pool")
  expect_equal(attr(p, "method"), "replicate")
  expect_equal(attr(p, "weights"), "inverse-variance")
  expect_true(all(p$n_ties == 2))
})

test_that("the pooled point estimate is the weighted mean of the ties'", {
  t <- two_ties()
  p <- pool_ties(siblings = t$siblings, household = t$household)

  a <- t$siblings$asdr.ind  %>% filter(alter.age == "[45,50)")
  b <- t$household$asdr.ind %>% filter(alter.age == "[45,50)")
  wa <- 1 / a$asdr.hat.se^2
  wb <- 1 / b$asdr.hat.se^2

  got <- p %>% filter(alter.age == "[45,50)")
  expect_equal(got$asdr.hat,
               (a$asdr.hat * wa + b$asdr.hat * wb) / (wa + wb))
})

test_that("the point estimate does not depend on which method computed the interval", {
  # only the uncertainty differs between the two methods
  t <- two_ties()
  a <- pool_ties(siblings = t$siblings, household = t$household)
  b <- suppressWarnings(pool_ties(siblings = t$siblings, household = t$household,
                                  method = "analytic"))
  expect_equal(a$asdr.hat, b$asdr.hat)
})

test_that("the analytic method warns that the ties are not independent", {
  t <- two_ties()
  expect_warning(
    pool_ties(siblings = t$siblings, household = t$household, method = "analytic"),
    "independent")
})

test_that("the analytic and replicate intervals differ", {
  # the whole reason the replicate path exists: assuming independence gives a
  # different answer from measuring the covariance. Which way it errs is a
  # property of the data, so this asserts only that it matters.
  t <- two_ties()
  a <- pool_ties(siblings = t$siblings, household = t$household)
  b <- suppressWarnings(pool_ties(siblings = t$siblings, household = t$household,
                                  method = "analytic"))

  ra <- a %>% filter(alter.age == "[45,50)")
  rb <- b %>% filter(alter.age == "[45,50)")
  expect_false(isTRUE(all.equal(ra$asdr.hat.se, rb$asdr.hat.se)))
})

test_that("equal weights give the plain average", {
  t <- two_ties()
  p <- pool_ties(siblings = t$siblings, household = t$household,
                 weights = "equal")

  a <- t$siblings$asdr.ind  %>% filter(alter.age == "[45,50)")
  b <- t$household$asdr.ind %>% filter(alter.age == "[45,50)")
  got <- p %>% filter(alter.age == "[45,50)")

  expect_equal(got$asdr.hat, mean(c(a$asdr.hat, b$asdr.hat)))
})

test_that("exposure weights use each tie's denominator", {
  t <- two_ties()
  p <- pool_ties(siblings = t$siblings, household = t$household,
                 weights = "exposure")

  a <- t$siblings$asdr.ind  %>% filter(alter.age == "[45,50)")
  b <- t$household$asdr.ind %>% filter(alter.age == "[45,50)")
  got <- p %>% filter(alter.age == "[45,50)")

  expect_equal(got$asdr.hat,
               (a$asdr.hat * a$denom.hat + b$asdr.hat * b$denom.hat) /
                 (a$denom.hat + b$denom.hat))
})

test_that("replicate pooling needs replicate estimates for every tie", {
  t <- two_ties()
  no.boot <- tie_est(tie_dat(2, rep(c(1,0), 8)), "household", return.boot = FALSE)

  expect_error(
    pool_ties(siblings = t$siblings, household = no.boot, method = "replicate"),
    "needs replicate-level estimates")
})

test_that("mismatched replicate counts are refused", {
  # pooling within replicates only means something if the replicates correspond
  t   <- two_ties()
  few <- tie_est(tie_dat(2, rep(c(1,0), 8)), "household", boot = tie_boot(M = 20))

  expect_error(
    pool_ties(siblings = t$siblings, household = few, method = "replicate"),
    "different numbers of bootstrap replicates")
})

test_that("inverse-variance weighting needs standard errors", {
  a <- tie_est(tie_dat(1, rep(c(1,0), 8)), "siblings", boot = NULL,
               return.boot = FALSE)
  b <- tie_est(tie_dat(2, rep(c(0,1), 8)), "household", boot = NULL,
               return.boot = FALSE)

  expect_error(pool_ties(siblings = a, household = b, method = "analytic"),
               "needs a standard error")
  # ... but equal weighting does not: the point estimate is still well defined,
  # and the absence of an interval is reported rather than fatal
  p <- suppressWarnings(suppressMessages(
    pool_ties(siblings = a, household = b, method = "analytic",
              weights = "equal")))
  expect_true(all(is.finite(p$asdr.hat)))
  expect_true(all(is.na(p$asdr.hat.se)))
})

# ---------------------------------------------------------------------------
# union
# ---------------------------------------------------------------------------
test_that("union without an alter key is not usable, and says why", {
  a <- tibble(.ego.id = c(1, 2), .sib.id = c("A", "B"))
  b <- tibble(.ego.id = c(1, 3), .sib.id = c("C", "D"))

  chk <- ties_union_check(maternal = a, paternal = b)
  expect_false(chk$usable)
  expect_match(chk$reason, "unique only within an ego")
})

test_that("a supplied key makes the overlap measurable", {
  a <- tibble(.ego.id = c(1, 1, 2), .sib.id = c("A", "B", "C"),
              person_id = c("p1", "p2", "p3"))
  b <- tibble(.ego.id = c(1, 3), .sib.id = c("D", "E"),
              person_id = c("p2", "p9"))

  chk <- ties_union_check(maternal = a, paternal = b, alter.key = "person_id")
  expect_true(chk$usable)
  expect_equal(chk$overlap$n_alters, 4)
  expect_equal(chk$overlap$n_shared, 1)     # p2 is in both
})

test_that("disjoint alter sets are reported as such", {
  a <- tibble(.ego.id = 1, .sib.id = "A", person_id = "p1")
  b <- tibble(.ego.id = 2, .sib.id = "B", person_id = "p2")

  chk <- ties_union_check(maternal = a, paternal = b, alter.key = "person_id")
  expect_equal(chk$overlap$n_shared, 0)
  expect_match(chk$reason, "disjoint")
})

test_that("a key missing from one tie is reported", {
  a <- tibble(.ego.id = 1, .sib.id = "A", person_id = "p1")
  b <- tibble(.ego.id = 2, .sib.id = "B")

  chk <- ties_union_check(maternal = a, paternal = b, alter.key = "person_id")
  expect_false(chk$usable)
  expect_match(chk$reason, "no column")
})

test_that("union needs at least two ties, named", {
  a <- tibble(.ego.id = 1, .sib.id = "A")
  expect_error(ties_union_check(maternal = a), "at least two")
  expect_error(ties_union_check(a, a), "must be named")
})

test_that("the union check says the union is not the structure of its parts", {
  # two cliques unioned are generally not a clique -- which is the socsim
  # finding, and the reason union needs its own tie_config()
  a <- tibble(.ego.id = 1, .sib.id = "A", person_id = "p1")
  b <- tibble(.ego.id = 2, .sib.id = "B", person_id = "p2")
  out <- paste(capture.output(
    print(ties_union_check(maternal = a, paternal = b, alter.key = "person_id"))),
    collapse = " ")
  expect_match(out, "not the structure of either part")
})
