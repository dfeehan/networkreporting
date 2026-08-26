# Applicability of a visibility rule is DECLARED, not inferred.
#
# The bug these tests exist for: vis_from_clique() returns a finite, plausible
# number for any roster, clique or not. Measured against socsim ground truth it
# is exact on siblings and overstates cousin visibility by 1.55x off-frame
# against 1.29x on-frame -- a differential that biases a death rate rather than
# cancelling out of it. Nothing in the data distinguishes the two cases, so the
# caller has to say which it is.

make_alters <- function() {
  dplyr::tibble(.ego.id   = c("a", "a", "b", "b"),
                .sib.id   = c("1", "2", "3", "4"),
                .sib.in.F = c(1, 0, 1, 1),
                y.F       = c(1, 1, 2, 2),
                .ego.weight = 1,
                sib.occ   = c(0, 1, 0, 0),
                sib.exp   = c(5, 2, 5, 5))
}

## donors for the approximating rule: the egos, with their own y.F
make_egos <- function() {
  dplyr::tibble(.ego.id = c("a", "b"), y.F = c(1, 2), .ego.weight = c(1, 1))
}

test_that("tie_config has no default structure", {
  expect_error(tie_config(), "deliberately has no default")
})

test_that("tie_config rejects a structure it does not know", {
  expect_error(tie_config("cliqe"), "must be one of")
  expect_error(tie_config(c("clique", "group")), "must be one of")
})

test_that("tie_config records structure and name", {
  tc <- tie_config("group", name = "maternal cousins")
  expect_true(is_tie_config(tc))
  expect_equal(tc$structure, "group")
  expect_equal(tc$name, "maternal cousins")
})

test_that("a clique rule refuses to run when no tie is declared", {
  expect_error(
    apply_visibility_rule(vis_from_clique(), make_alters()),
    "no tie was declared")
})

test_that("a clique rule refuses a tie declared as something else", {
  for (s in c("group", "star", "unbounded")) {
    expect_error(
      apply_visibility_rule(vis_from_clique(), make_alters(),
                            tie = tie_config(s)),
      "declared '")
  }
})

test_that("a clique rule runs, unchanged, on a tie declared a clique", {
  gated <- apply_visibility_rule(vis_from_clique(), make_alters(),
                                 tie = tie_config("clique"))
  ## the values are exactly what the rule produces on its own
  raw <- vis_from_clique()$predict(make_alters(),
                                   list(ego.in.group = TRUE))
  expect_equal(gated$values$vis, raw$vis)
  expect_equal(gated$values$vis_weight, raw$vis_weight)
})

test_that("a rule making no structural assumption needs no tie", {
  expect_silent(
    res <- apply_visibility_rule(vis_from_donor(match_on = NULL, min_donors = 1),
                                 make_alters(), ego.dat = make_egos(),
                                 weights = ".ego.weight"))
  expect_true(all(!is.na(res$values$vis)))
})

test_that("a donor rule accepts any declared structure", {
  for (s in c("clique", "group", "star", "unbounded")) {
    res <- apply_visibility_rule(vis_from_donor(match_on = NULL, min_donors = 1),
                                 make_alters(), ego.dat = make_egos(),
                                 weights = ".ego.weight", tie = tie_config(s))
    expect_true(all(!is.na(res$values$vis)))
  }
})

test_that("coalesce drops the clique tier on a non-clique tie, and says so", {
  res <- apply_visibility_rule(
    vis_coalesce(vis_from_clique(), vis_from_donor(match_on = NULL, min_donors = 1)),
    make_alters(), ego.dat = make_egos(), weights = ".ego.weight",
    tie = tie_config("group", name = "cousins"))

  ## the clique tier is REMOVED because it is inapplicable -- not skipped
  ## because it returned NA. It never returns NA; that was the bug.
  expect_equal(res$provenance$dropped_tiers, "clique")
  expect_false(any(res$values$vis_rule == "clique"))
  expect_equal(res$provenance$tie, "group")
  expect_equal(res$provenance$tie_name, "cousins")
})

test_that("coalesce keeps every tier on a clique tie", {
  res <- apply_visibility_rule(
    vis_coalesce(vis_from_clique(), vis_from_donor(match_on = NULL, min_donors = 1)),
    make_alters(), ego.dat = make_egos(), weights = ".ego.weight",
    tie = tie_config("clique"))

  expect_null(res$provenance$dropped_tiers)
  ## the exact tier claims everything, since it never returns NA
  expect_true(all(res$values$vis_rule == "clique"))
})

test_that("a chain of only-clique tiers is refused whole on a non-clique tie", {
  ## A coalesce inherits the union of its tiers' applicability, so a chain in
  ## which every tier is clique-only is itself clique-only and is refused by the
  ## top-level check -- before there is any question of dropping tiers. The
  ## "no tier applies" branch inside restrict_to_tie() is a defensive guard for
  ## callers that reach it directly.
  expect_error(
    apply_visibility_rule(
      vis_coalesce(vis_from_clique(), vis_from_clique(ego.in.group = FALSE)),
      make_alters(), ego.dat = make_egos(), weights = ".ego.weight",
      tie = tie_config("unbounded")),
    "only valid for tie structure")
})

test_that("restrict_to_tie() refuses directly when nothing applies", {
  expect_error(
    restrict_to_tie(vis_coalesce(vis_from_clique(),
                                 vis_from_clique(ego.in.group = FALSE)),
                    tie_config("unbounded")),
    "no tier of")
})

test_that("tie must be a tie_config", {
  expect_error(
    apply_visibility_rule(vis_from_clique(), make_alters(), tie = "clique"),
    "must be a tie_config")
})

test_that("the refusal message names what to do instead", {
  msg <- tryCatch(
    apply_visibility_rule(vis_from_clique(), make_alters(),
                          tie = tie_config("group")),
    error = conditionMessage)
  expect_match(msg, "vis_from_donor")
  expect_match(msg, "vis_coalesce")
})
