# Tests for the settings that belong to a tie rather than to a rule:
# ego.in.group and frame.indicator.
#
# The design point being tested throughout is that a declared value is used, and
# that two explicitly-set values which disagree are an ERROR rather than one
# silently winning. Silent precedence would reintroduce the failure the tie gate
# exists to prevent -- a number computed under an assumption the caller did not
# know was in force.

library(tibble)

# ego 1 reports A (on frame) and B (off frame); ego 2 reports C (on frame).
# Every ego has y.F = 1, which keeps the arithmetic easy to read:
#   ego.in.group = TRUE  -> vis = y.F + 1 - in.F = 2 - in.F  -> 1, 2, 1
#   ego.in.group = FALSE -> vis = y.F     - in.F = 1 - in.F  -> 0, 1, 0
tie_dat <- function(frame.col = ".sib.in.F") {
  d <- tibble(.ego.id = c(1, 1, 2),
              .sib.id = c("A", "B", "C"),
              y.F     = c(1, 1, 1))
  d[[frame.col]] <- c(1, 0, 1)
  d
}

clique_tie <- function(...) tie_config("clique", name = "siblings", ...)

# ---------------------------------------------------------------------------
# ego.in.group
# ---------------------------------------------------------------------------
test_that("a tie's ego.in.group is used when the rule does not set one", {
  got <- apply_visibility_rule(vis_from_clique(), tie_dat(),
                               tie = clique_tie(ego.in.group = FALSE))
  expect_equal(got$values$vis, c(0, 1, 0))
})

test_that("the rule's ego.in.group is used when the tie does not declare one", {
  # unchanged behaviour: this is how it worked before the setting moved
  got <- apply_visibility_rule(vis_from_clique(ego.in.group = FALSE), tie_dat(),
                               tie = clique_tie())
  expect_equal(got$values$vis, c(0, 1, 0))
})

test_that("with neither declaring, the default is unchanged", {
  got <- apply_visibility_rule(vis_from_clique(), tie_dat(), tie = clique_tie())
  expect_equal(got$values$vis, c(1, 2, 1))
})

test_that("two declarations that agree are accepted", {
  got <- apply_visibility_rule(vis_from_clique(ego.in.group = FALSE), tie_dat(),
                               tie = clique_tie(ego.in.group = FALSE))
  expect_equal(got$values$vis, c(0, 1, 0))
})

test_that("two declarations that disagree are an error, not a silent precedence", {
  expect_error(
    apply_visibility_rule(vis_from_clique(ego.in.group = TRUE), tie_dat(),
                          tie = clique_tie(ego.in.group = FALSE)),
    "conflicting values for 'ego.in.group'")
})

test_that("the conflict message names both sources and their values", {
  msg <- tryCatch(
    apply_visibility_rule(vis_from_clique(ego.in.group = TRUE), tie_dat(),
                          tie = clique_tie(ego.in.group = FALSE)),
    error = function(e) conditionMessage(e))

  expect_match(msg, "tie_config\\(\\) declares: FALSE")
  expect_match(msg, "sets: TRUE")
})

test_that("apply_visibility_rule's own ego.in.group argument is reconciled too", {
  # this argument governs how y.F is derived; the rule's governs what is done
  # with it. Before this, nothing made the two agree.
  expect_error(
    apply_visibility_rule(vis_from_clique(), tie_dat(), ego.in.group = TRUE,
                          tie = clique_tie(ego.in.group = FALSE)),
    "conflicting values for 'ego.in.group'")
})

test_that("the assumptions reported match the setting that was actually used", {
  # provenance must not claim an assumption a tie declaration overrode
  yes <- apply_visibility_rule(vis_from_clique(), tie_dat(),
                               tie = clique_tie(ego.in.group = TRUE))$provenance
  no  <- apply_visibility_rule(vis_from_clique(), tie_dat(),
                               tie = clique_tie(ego.in.group = FALSE))$provenance

  expect_true(any(grepl("^ego is a member", yes$assumptions)))
  expect_false(any(grepl("^ego is a member", no$assumptions)))
  expect_true(any(grepl("NOT a member", no$assumptions)))

  expect_true(yes$ego_in_group)
  expect_false(no$ego_in_group)
})

# ---------------------------------------------------------------------------
# frame.indicator
# ---------------------------------------------------------------------------
test_that("a tie can name the frame indicator column", {
  got <- apply_visibility_rule(vis_from_clique(), tie_dat("in.bari"),
                               tie = clique_tie(frame.indicator = "in.bari"))
  expect_equal(got$values$vis, c(1, 2, 1))
})

test_that("the argument still works, and still wins when the tie is silent", {
  got <- apply_visibility_rule(vis_from_clique(), tie_dat("in.bari"),
                               frame.indicator = "in.bari",
                               tie = clique_tie())
  expect_equal(got$values$vis, c(1, 2, 1))
})

test_that("a frame indicator declared in both places must agree", {
  expect_error(
    apply_visibility_rule(vis_from_clique(), tie_dat("in.bari"),
                          frame.indicator = "in.bari",
                          tie = clique_tie(frame.indicator = ".sib.in.F")),
    "conflicting values for 'frame.indicator'")
})

test_that("a non-default frame indicator is reported in provenance", {
  p <- apply_visibility_rule(vis_from_clique(), tie_dat("in.bari"),
                             tie = clique_tie(frame.indicator = "in.bari"))$provenance
  expect_equal(p$frame_indicator, "in.bari")
})

# ---------------------------------------------------------------------------
# tie_config itself
# ---------------------------------------------------------------------------
test_that("tie_config keeps its new fields undeclared by default", {
  tc <- tie_config("clique", name = "siblings")
  expect_null(tc$ego.in.group)
  expect_null(tc$frame.indicator)
})

test_that("tie_config validates the types of its new arguments", {
  expect_error(tie_config("clique", ego.in.group = "yes"), "TRUE, FALSE, or NULL")
  expect_error(tie_config("clique", ego.in.group = NA), "TRUE, FALSE, or NULL")
  expect_error(tie_config("clique", frame.indicator = 1), "single column name")
})

test_that("structure and ego.in.group may disagree, deliberately", {
  # a household roster that excludes the respondent is a clique the respondent
  # is outside of; the Matlab rosters carry the respondent as a row, so their
  # count is yprime.F rather than y.F. Both are real, so neither is refused.
  expect_s3_class(tie_config("clique", ego.in.group = FALSE), "tie_config")
  expect_s3_class(tie_config("star",   ego.in.group = TRUE),  "tie_config")
})

test_that("printing a tie says which settings are undeclared", {
  out <- paste(capture.output(print(tie_config("clique", name = "siblings"))),
               collapse = "\n")
  expect_match(out, "not declared")

  out2 <- paste(capture.output(print(
    tie_config("star", name = "parents",
               ego.in.group = FALSE, frame.indicator = "in.F"))), collapse = "\n")
  expect_match(out2, "FALSE")
  expect_match(out2, "in.F")
})

# ---------------------------------------------------------------------------
# coalesced rules
# ---------------------------------------------------------------------------
test_that("a tie setting reaches every tier of a coalesced rule", {
  chain <- vis_coalesce(vis_from_clique(),
                        vis_from_donor(match_on = NULL, min_donors = 1))

  # vis_coalesce() fits every tier up front, including ones no row will need,
  # so the donor tier still wants donors
  egos <- tibble(.ego.id = c(1, 2), y.F = c(1, 1), w = c(1, 1))
  got  <- apply_visibility_rule(chain, tie_dat(), ego.dat = egos, weights = "w",
                                tie = clique_tie(ego.in.group = FALSE))

  # the clique tier resolves everything, using the tie's setting
  expect_equal(got$values$vis, c(0, 1, 0))
})

test_that("a conflict inside a coalesced tier is still an error", {
  chain <- vis_coalesce(vis_from_clique(ego.in.group = TRUE),
                        vis_from_donor(match_on = NULL, min_donors = 1))

  egos <- tibble(.ego.id = c(1, 2), y.F = c(1, 1), w = c(1, 1))
  expect_error(
    apply_visibility_rule(chain, tie_dat(), ego.dat = egos, weights = "w",
                          tie = clique_tie(ego.in.group = FALSE)),
    "conflicting values for 'ego.in.group'")
})

# ---------------------------------------------------------------------------
# nothing was foreclosed
# ---------------------------------------------------------------------------
test_that("a rule that makes no structural assumption still needs no tie", {
  egos <- tibble(.ego.id = c(1, 2), y.F = c(1, 1), w = c(1, 1))
  got  <- apply_visibility_rule(vis_from_donor(match_on = NULL, min_donors = 1),
                                tie_dat(), ego.dat = egos, weights = "w")
  expect_true(all(is.finite(got$values$vis)))
})
