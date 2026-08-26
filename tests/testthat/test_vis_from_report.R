# Tests for vis_from_report(): visibility as the respondent reported it.
#
# The distinguishing feature of this rule is that it assumes nothing at all
# about the tie -- which is what makes it the only option for an "unbounded"
# tie, where there is no bounded group to count and no reason to think the
# respondents resemble the alters.
#
# What it assumes instead is that the answer means what the analyst thinks it
# means, and the two arguments that encode that (counts.ego, counts.self) are
# decided by the wording of the question rather than by anything in the data.

library(tibble)

report_dat <- function() {
  tibble(.ego.id   = 1:5,
         .sib.in.F = c(1, 0, 1, 0, 1),
         # "how many people like you does this person know?" -- includes ego
         n_known   = c(4, 3, 2, 5, 2),
         # "...how many OTHER people like you?" -- excludes ego
         n_other   = c(3, 2, 1, 4, 1))
}

predict_report <- function(rule, d = report_dat()) rule$predict(d, rule$fit(NULL, NULL))

# ---------------------------------------------------------------------------
# The straightforward reading
# ---------------------------------------------------------------------------
test_that("a reported degree is used as the visibility, unchanged", {
  got <- predict_report(vis_from_report("n_known"))
  expect_equal(got$vis, c(4, 3, 2, 5, 2))
  expect_equal(got$vis_weight, 1 / c(4, 3, 2, 5, 2))
})

test_that("counts.ego = FALSE adds the respondent back", {
  # the two questions describe the same quantity, asked differently, so they
  # must produce the same visibility
  a <- predict_report(vis_from_report("n_known"))
  b <- predict_report(vis_from_report("n_other", counts.ego = FALSE))
  expect_equal(a$vis, b$vis)
})

test_that("counts.self = TRUE subtracts an on-frame alter, like a group size", {
  # a question phrased about a GROUP counts the alter; one phrased about
  # connections does not
  d   <- report_dat()
  got <- predict_report(vis_from_report("n_known", counts.self = TRUE), d)
  expect_equal(got$vis, c(4, 3, 2, 5, 2) - c(1, 0, 1, 0, 1))
})

test_that("with counts.self it agrees with vis_from_group_size", {
  # the same semantics reached two ways; vis_from_group_size() is the clearer
  # constructor for it, and the docs say so
  d <- report_dat()
  a <- predict_report(vis_from_report("n_known", counts.self = TRUE), d)
  b <- vis_from_group_size("n_known")$predict(d, list(ego.in.group = TRUE))
  expect_equal(a$vis, b$vis)
})

# ---------------------------------------------------------------------------
# It assumes nothing about the tie
# ---------------------------------------------------------------------------
test_that("the rule needs no tie and accepts any structure", {
  # the point of it: an unbounded tie has no roster to derive from, so this is
  # the only rule that can serve one honestly
  d <- report_dat()
  expect_silent(apply_visibility_rule(vis_from_report("n_known"), d))

  for (st in c("clique", "group", "star", "unbounded")) {
    got <- apply_visibility_rule(vis_from_report("n_known"), d,
                                 tie = tie_config(st, name = "t"))
    expect_equal(got$values$vis, c(4, 3, 2, 5, 2))
  }
})

test_that("it is not estimated, so the bootstrap freezes it", {
  # the report is data, not something fitted to the sample
  expect_false(vis_from_report("n_known")$is_estimated)
})

# ---------------------------------------------------------------------------
# Impossible and missing answers
# ---------------------------------------------------------------------------
test_that("a reported visibility below one contradicts the report itself", {
  # this alter was named by a respondent, so at least one frame member saw them
  d <- report_dat()
  d$n_known[2] <- 0
  expect_error(predict_report(vis_from_report("n_known"), d), "below 1")
})

test_that("that message names the two usual causes", {
  d <- report_dat()
  d$n_known[2] <- 0
  msg <- tryCatch(predict_report(vis_from_report("n_known"), d),
                  error = function(e) conditionMessage(e))
  expect_match(msg, "don't-know code stored as 0")
  expect_match(msg, "counts.ego")
})

test_that("on_impossible = 'floor' raises it to one", {
  d <- report_dat()
  d$n_known[2] <- 0
  got <- predict_report(vis_from_report("n_known", on_impossible = "floor"), d)
  expect_equal(got$vis[2], 1)
  expect_equal(got$vis[-2], c(4, 2, 5, 2))
})

test_that("on_impossible = 'na' leaves it for another tier", {
  d <- report_dat()
  d$n_known[2] <- 0
  got <- predict_report(vis_from_report("n_known", on_impossible = "na"), d)
  expect_true(is.na(got$vis[2]))
  expect_true(is.na(got$vis_rule[2]))
})

test_that("item non-response errors by default, and can fall through", {
  d <- report_dat()
  d$n_known[3] <- NA

  expect_error(predict_report(vis_from_report("n_known"), d), "no reported visibility")

  got <- predict_report(vis_from_report("n_known", on_missing = "na"), d)
  expect_true(is.na(got$vis[3]))
})

test_that("the non-response message suggests coalescing rather than stopping", {
  # item non-response on this question is normal, so an error here is usually a
  # sign the rule wants a fallback tier, not that the data is broken
  d <- report_dat()
  d$n_known[3] <- NA
  msg <- tryCatch(predict_report(vis_from_report("n_known"), d),
                  error = function(e) conditionMessage(e))
  expect_match(msg, "vis_coalesce")
})

# ---------------------------------------------------------------------------
# Arguments and provenance
# ---------------------------------------------------------------------------
test_that("report.var is required and must name one column", {
  expect_error(vis_from_report(), "needs report.var")
  expect_error(vis_from_report(c("a", "b")), "needs report.var")
})

test_that("a missing report column is caught up front", {
  expect_error(apply_visibility_rule(vis_from_report("no_such_col"), report_dat()),
               "no_such_col")
})

test_that("the assumptions say where the uncertainty went", {
  # unlike the other rules, this one puts it in reporting error rather than in
  # an assumption, and the provenance should say so
  a <- vis_from_report("n_known")$assumptions
  expect_true(any(grepl("reporting error", a)))
  expect_true(any(grepl("includes the respondent", a)))

  b <- vis_from_report("n_other", counts.ego = FALSE)$assumptions
  expect_true(any(grepl("excludes the respondent", b)))
})

test_that("the label names the column, or whatever the caller called it", {
  expect_equal(vis_from_report("n_known")$label, "reported(n_known)")
  expect_equal(vis_from_report("n_known", label = "asked")$label, "asked")
})

# ---------------------------------------------------------------------------
# Coalescing: report where asked, approximate where not
# ---------------------------------------------------------------------------
test_that("it coalesces ahead of a donor tier", {
  # the intended shape where the question was asked of only some respondents
  d <- tibble(.ego.id   = c(1, 2),
              .sib.in.F = c(0, 0),
              n_known   = c(4, NA),
              y.F       = c(2, 2))
  egos <- tibble(.ego.id = c(1, 2), y.F = c(2, 2), w = c(1, 1))

  chain <- vis_coalesce(vis_from_report("n_known", on_missing = "na"),
                        vis_from_donor(match_on = NULL, min_donors = 1))
  got   <- apply_visibility_rule(chain, d, ego.dat = egos, weights = "w")

  expect_equal(got$values$vis[1], 4)          # reported
  expect_equal(got$values$vis_tier, c(1L, 2L))
  expect_equal(got$values$vis[2], 3)          # donor: S.hat = y.F + 1
})

test_that("a chain of report then clique is not estimated", {
  # neither tier fits anything to the sample, so the bootstrap may freeze both
  chain <- vis_coalesce(vis_from_report("n_known", on_missing = "na"),
                        vis_from_clique())
  expect_false(chain$is_estimated)
})
