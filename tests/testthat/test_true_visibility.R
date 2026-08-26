# Tests for the canonical true-visibility definition and the scoring built on
# it.
#
# The definition is short enough that it kept getting written inline, and kept
# getting written differently. Two things go wrong, and neither announces
# itself, so both have tests of their own:
#
#   1. whose frame membership counts -- the REPORTER's, never the alter's
#   2. duplicate (reporter, alter) pairs, which double the answer
#
# Verified against the socsim Bangladesh networks outside the test suite, where
# it reproduces the hand-rolled truth exactly on 30,193 sibling alters and
# 49,886 cousin-union alters, and visibility_accuracy() reproduces the
# published differentials of 1.000 and 1.026.

library(tibble)

# x is connected to three people, two of them frame members;
# y is connected to one, listed twice (a census linking a pair by two routes)
truth_census <- function() {
  tibble(from          = c("a", "b", "c", "a", "a"),
         to            = c("x", "x", "x", "y", "y"),
         from_in_frame = c(1, 1, 0, 1, 1))
}

# ---------------------------------------------------------------------------
# Whose frame membership
# ---------------------------------------------------------------------------
test_that("visibility counts frame-member REPORTERS", {
  got <- suppressMessages(
    true_visibility_from_network(truth_census(), "from", "to", "from_in_frame"))

  # x: a and b are in the frame, c is not -> 2
  # y: a only, and the duplicate pair counts once -> 1
  expect_equal(got$vis_true[got$to == "x"], 2L)
  expect_equal(got$vis_true[got$to == "y"], 1L)
})

test_that("filtering on the alter's frame status gives a different answer", {
  # this is the confusion the function exists to settle: an alter's visibility
  # depends on who could report THEM, so it is the reporter's status that
  # matters. Scoring by the alter's is a plausible number and a wrong one.
  d <- truth_census()
  d$to_in_frame <- ifelse(d$to == "x", 0, 1)     # x is NOT in the frame

  by.reporter <- suppressMessages(
    true_visibility_from_network(d, "from", "to", "from_in_frame"))
  by.alter <- suppressMessages(
    true_visibility_from_network(d, "from", "to", "to_in_frame"))

  expect_false(isTRUE(all.equal(by.reporter$vis_true, by.alter$vis_true)))
  # x is reportable by two frame members, but is not itself in the frame
  expect_equal(by.reporter$vis_true[by.reporter$to == "x"], 2L)
  expect_equal(nrow(by.alter[by.alter$to == "x", ]), 0L)
})

test_that("with no frame column every reporter counts", {
  # the usual case for a census already restricted to frame members
  got <- suppressMessages(
    true_visibility_from_network(truth_census(), "from", "to"))
  expect_equal(got$vis_true[got$to == "x"], 3L)   # c now counts too
})

# ---------------------------------------------------------------------------
# Duplicate pairs
# ---------------------------------------------------------------------------
test_that("repeated pairs are dropped, and reported", {
  expect_message(
    true_visibility_from_network(truth_census(), "from", "to", "from_in_frame"),
    "duplicate")
})

test_that("keeping duplicates doubles the answer, which is the bug", {
  # in one socsim census this made an exact rule look 27% wrong
  keep <- suppressMessages(
    true_visibility_from_network(truth_census(), "from", "to", "from_in_frame",
                                 dedup = FALSE))
  expect_equal(keep$vis_true[keep$to == "y"], 2L)   # the same pair, twice
})

test_that("a census with no duplicates says nothing about them", {
  d <- truth_census()[1:4, ]
  expect_silent(true_visibility_from_network(d, "from", "to", "from_in_frame"))
})

# ---------------------------------------------------------------------------
# Alters no frame member can reach
# ---------------------------------------------------------------------------
test_that("an alter with no frame-member reporter is reported and omitted", {
  d <- tibble(from = c("a", "b"), to = c("x", "z"), from_in_frame = c(1, 0))

  expect_message(true_visibility_from_network(d, "from", "to", "from_in_frame"),
                 "no frame-population member")

  got <- suppressMessages(
    true_visibility_from_network(d, "from", "to", "from_in_frame"))
  expect_false("z" %in% got$to)
})

test_that("a missing column is named against what the census has", {
  expect_error(
    true_visibility_from_network(truth_census(), "from", "nope"),
    "not in the census")
})

# ---------------------------------------------------------------------------
# Scoring
# ---------------------------------------------------------------------------
test_that("accuracy is reported separately for each side of the frame split", {
  acc <- visibility_accuracy(predicted = c(3, 4, 3, 4),
                             truth     = c(3, 3, 3, 4),
                             in.frame  = c(TRUE, FALSE, TRUE, FALSE))

  on  <- acc$by_side[acc$by_side$side == "on-frame", ]
  off <- acc$by_side[acc$by_side$side == "off-frame", ]

  expect_equal(on$exact, 1)                 # both on-frame predictions exact
  expect_equal(on$mean_ratio, 1)
  expect_equal(off$mean_ratio, mean(c(4/3, 4/4)))
})

test_that("the differential is the ratio between the two sides", {
  acc <- visibility_accuracy(predicted = c(2, 3), truth = c(1, 3),
                             in.frame  = c(FALSE, TRUE))
  expect_equal(acc$differential, 2 / 1)
})

test_that("a uniform error gives a differential of one", {
  # the point of the whole split: an error of the same size on both sides
  # largely cancels out of a rate
  acc <- visibility_accuracy(predicted = c(4, 6, 8), truth = c(2, 3, 4),
                             in.frame  = c(TRUE, FALSE, TRUE))
  expect_equal(acc$differential, 1)
})

test_that("a perfect rule scores exactly", {
  acc <- visibility_accuracy(predicted = c(1, 2, 3), truth = c(1, 2, 3),
                             in.frame  = c(TRUE, FALSE, TRUE))
  expect_equal(acc$differential, 1)
  expect_true(all(acc$by_side$exact == 1))
})

test_that("rows that cannot be scored are dropped and counted", {
  # a true visibility of zero would divide by zero; a missing prediction has
  # nothing to score
  acc <- visibility_accuracy(predicted = c(2, NA, 3, 4),
                             truth     = c(1, 2, 0, 4),
                             in.frame  = c(FALSE, TRUE, TRUE, TRUE))
  expect_equal(acc$n_scored, 2)
  expect_equal(acc$n_dropped, 2)
})

test_that("mismatched lengths are refused", {
  expect_error(visibility_accuracy(1:3, 1:2, c(TRUE, FALSE)),
               "same length")
})

test_that("nothing scorable is an error rather than a silent NaN", {
  expect_error(visibility_accuracy(c(NA, NA), c(1, 2), c(TRUE, FALSE)),
               "nothing left to score")
})

test_that("the printed output says which way the differential cuts", {
  out <- paste(capture.output(print(
    visibility_accuracy(c(2, 3), c(1, 3), c(FALSE, TRUE)))), collapse = " ")
  expect_match(out, "BIASES a rate")

  out2 <- paste(capture.output(print(
    visibility_accuracy(c(1, 2), c(1, 2), c(FALSE, TRUE)))), collapse = " ")
  expect_match(out2, "cancels out of a rate")
})

# ---------------------------------------------------------------------------
# The two together
# ---------------------------------------------------------------------------
test_that("the clique rule scores perfectly on a genuine clique", {
  # three siblings, all frame members, each reporting the other two. Every
  # alter is visible to 2 frame members (the other two), and the clique rule
  # should say so exactly.
  census <- tibble(
    from = c("a", "a", "b", "b", "c", "c"),
    to   = c("b", "c", "a", "c", "a", "b"),
    from_in_frame = 1)

  truth <- suppressMessages(
    true_visibility_from_network(census, "from", "to", "from_in_frame"))
  expect_true(all(truth$vis_true == 2))

  # y.F = 2 for each ego; every alter is on frame, so the rule gives y.F = 2
  reports <- census
  reports$.sib.in.F <- 1
  reports$y.F <- 2
  pred <- vis_from_clique()$predict(reports, list(ego.in.group = TRUE))$vis

  scored <- merge(reports, truth, by.x = "to", by.y = "to")
  acc <- visibility_accuracy(rep(2, nrow(scored)), scored$vis_true,
                             scored$.sib.in.F)
  expect_equal(acc$by_side$mean_ratio, 1)
})
