# Tests for vis_from_group_size(): visibility from a group the caller supplies
# rather than one derived from ego's roster.
#
# The motivating cases are the cousin visibility bases in the Matlab multi-tie
# study, which are the same arithmetic applied to differently-defined groups --
# pooled across maternal and paternal sides, or pooled minus the sibship. None
# of those groups is any single roster, so none can be derived the way
# vis_from_clique() derives one.

library(tibble)

# A roster that carries the respondent as a row, so the count already includes
# ego -- the package's yprime.F rather than y.F. This is how the Matlab rosters
# are built.
group_dat <- function() {
  d <- tibble(.ego.id   = c(1, 1, 2),
              .sib.id   = c("A", "B", "C"),
              .sib.in.F = c(1, 0, 1),
              n_pooled_and_F  = c(5, 5, 3),
              n_sibship_and_F = c(2, 2, 1))
  d$n_pooled_nosib_and_F <- d$n_pooled_and_F - d$n_sibship_and_F
  d
}

predict_with <- function(rule, d = group_dat()) {
  rule$predict(d, rule$fit(NULL, NULL))
}

# ---------------------------------------------------------------------------
# The three cousin bases
# ---------------------------------------------------------------------------
# All are G_F - in.F, on differently-defined groups, except the third which
# omits the frame split.
test_that("the pooled-cousins basis is reproduced exactly", {
  got <- predict_with(vis_from_group_size("n_pooled_and_F"))
  # 5 - 1, 5 - 0, 3 - 1
  expect_equal(got$vis, c(4, 5, 2))
})

test_that("the pooled-without-siblings basis is reproduced exactly", {
  # the caller supplies the difference as a column; that basis applies no
  # frame split, so subtract.self is off
  got <- predict_with(vis_from_group_size("n_pooled_nosib_and_F",
                                          subtract.self = FALSE))
  expect_equal(got$vis, c(3, 3, 2))
})

test_that("a label makes provenance say which basis produced the estimate", {
  got <- predict_with(vis_from_group_size("n_pooled_and_F",
                                          label = "pooled cousins"))
  expect_equal(unique(got$vis_rule), "pooled cousins")

  # and without one, the label still names the column it came from
  auto <- predict_with(vis_from_group_size("n_pooled_and_F"))
  expect_equal(unique(auto$vis_rule), "group_size(n_pooled_and_F)")
})

# ---------------------------------------------------------------------------
# Agreement with the exact rule
# ---------------------------------------------------------------------------
test_that("given the clique's own group size, it reproduces vis_from_clique", {
  # this is the check that anchors the new rule to the exact one: hand it the
  # group vis_from_clique() would have derived, and it must agree
  d <- tibble(.ego.id   = c(1, 1, 2),
              .sib.in.F = c(1, 0, 1),
              y.F       = c(2, 2, 1))
  d$yprime.F <- d$y.F + 1        # the roster including ego

  clique <- vis_from_clique()
  a <- clique$predict(d, clique$fit(NULL, NULL))
  b <- predict_with(vis_from_group_size("yprime.F"), d)

  expect_equal(a$vis, b$vis)
  expect_equal(a$vis_weight, b$vis_weight)
})

test_that("counts.ego = FALSE adds ego back, matching y.F semantics", {
  d <- tibble(.ego.id = c(1, 1, 2), .sib.in.F = c(1, 0, 1), y.F = c(2, 2, 1))

  clique <- vis_from_clique()
  a <- clique$predict(d, clique$fit(NULL, NULL))
  b <- predict_with(vis_from_group_size("y.F", counts.ego = FALSE), d)

  expect_equal(a$vis, b$vis)
})

test_that("counts.ego = FALSE with ego outside the group does not add", {
  d <- tibble(.ego.id = c(1, 1), .sib.in.F = c(1, 0), n = c(4, 4))
  got <- predict_with(vis_from_group_size("n", counts.ego = FALSE,
                                          ego.in.group = FALSE), d)
  expect_equal(got$vis, c(3, 4))
})

# ---------------------------------------------------------------------------
# The frame split
# ---------------------------------------------------------------------------
test_that("the frame split is on by default and is the whole difference", {
  d <- tibble(.ego.id = c(1, 1), .sib.in.F = c(1, 0), n = c(4, 4))

  split <- predict_with(vis_from_group_size("n"), d)
  flat  <- predict_with(vis_from_group_size("n", subtract.self = FALSE), d)

  expect_equal(split$vis, c(3, 4))
  expect_equal(flat$vis,  c(4, 4))
})

test_that("turning the frame split off is recorded in the assumptions", {
  on  <- vis_from_group_size("n")
  off <- vis_from_group_size("n", subtract.self = FALSE)

  expect_true(any(grepl("does not count themselves", on$assumptions)))
  expect_true(any(grepl("DOES count themselves", off$assumptions)))
  # and the consequence is spelled out, not just the fact
  expect_true(any(grepl("cancels out of a rate", off$assumptions)))
})

test_that("the caller's responsibility for the column is stated", {
  r <- vis_from_group_size("n_pooled_and_F")
  expect_true(any(grepl("the package does not check", r$assumptions)))
})

# ---------------------------------------------------------------------------
# Structural assumptions, and the lack of them
# ---------------------------------------------------------------------------
test_that("the rule needs no tie, because the caller supplied the group", {
  # unlike vis_from_clique(), it asserts nothing about the tie structure, so it
  # runs without a declaration and against any structure
  d <- group_dat()
  expect_silent(apply_visibility_rule(vis_from_group_size("n_pooled_and_F"), d))

  for (st in c("clique", "group", "star", "unbounded")) {
    got <- apply_visibility_rule(vis_from_group_size("n_pooled_and_F"), d,
                                 tie = tie_config(st, name = "t"))
    expect_equal(got$values$vis, c(4, 5, 2))
  }
})

test_that("it is not estimated, so the bootstrap freezes it", {
  # the column is read off the data, not fitted to the sample
  expect_false(vis_from_group_size("n")$is_estimated)
})

test_that("a missing size column is reported up front", {
  d <- group_dat()
  expect_error(
    apply_visibility_rule(vis_from_group_size("no_such_column"), d),
    "no_such_column")
})

test_that("size.var is required, and must be a single column name", {
  expect_error(vis_from_group_size(), "needs size.var")
  expect_error(vis_from_group_size(c("a", "b")), "needs size.var")
  expect_error(vis_from_group_size(42), "needs size.var")
})

# ---------------------------------------------------------------------------
# Working with the rest of the layer
# ---------------------------------------------------------------------------
test_that("a tie may declare ego.in.group for this rule too", {
  d <- tibble(.ego.id = c(1, 1), .sib.in.F = c(1, 0), n = c(4, 4))

  got <- apply_visibility_rule(
    vis_from_group_size("n", counts.ego = FALSE), d,
    tie = tie_config("group", name = "cousins", ego.in.group = FALSE))

  expect_equal(got$values$vis, c(3, 4))
})

test_that("it coalesces with other rules", {
  # a group size known for some alters and not others: the supplied-group tier
  # takes what it can and the donor tier picks up the rest
  d <- tibble(.ego.id   = c(1, 2),
              .sib.in.F = c(0, 0),
              n         = c(4, NA))
  egos <- tibble(.ego.id = c(1, 2), y.F = c(2, 2), w = c(1, 1))

  chain <- vis_coalesce(vis_from_group_size("n"),
                        vis_from_donor(match_on = NULL, min_donors = 1))
  got <- apply_visibility_rule(chain, d, ego.dat = egos, weights = "w")

  expect_equal(got$values$vis[1], 4)
  expect_equal(got$values$vis_tier, c(1L, 2L))
  expect_false(is.na(got$values$vis[2]))
})

test_that("several bases can be compared, each naming itself in provenance", {
  # this is the point of the label: run the same cells under competing bases
  # and have the output say which is which
  d <- group_dat()

  bases <- list(
    vis_from_group_size("n_pooled_and_F",       label = "pooled"),
    vis_from_group_size("n_pooled_nosib_and_F", label = "pooled w/o sibs",
                        subtract.self = FALSE))

  out <- lapply(bases, function(b) apply_visibility_rule(b, d))

  expect_equal(out[[1]]$provenance$rule, "pooled")
  expect_equal(out[[2]]$provenance$rule, "pooled w/o sibs")
  expect_false(isTRUE(all.equal(out[[1]]$values$vis, out[[2]]$values$vis)))
})
