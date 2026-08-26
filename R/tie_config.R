##' Declare what kind of tie a set of reports is about
##'
##' A visibility rule is only valid for certain kinds of tie, and **which kind a
##' tie is cannot be worked out from the data**. `tie_config()` is how the caller
##' says so, and [apply_visibility_rule()] refuses to apply a rule outside the
##' structures it is valid for.
##'
##' @section Why this is required rather than defaulted:
##'
##' [vis_from_clique()] computes `y.F` for an on-frame alter and `y.F + 1`
##' otherwise. That is a *theorem* about cliques, not a definition of visibility:
##' it follows only because the tie partitions the population into disjoint
##' groups, ego belongs to the group ego reports about, and reporting within the
##' group is complete.
##'
##' Given a roster of reports, nothing distinguishes a tie that satisfies those
##' conditions from one that does not. The rule returns a finite, plausible
##' number either way. Applied to a non-clique tie it therefore produces a wrong
##' answer with no outward sign of trouble --- which is why the structure has to
##' be stated rather than inferred or defaulted.
##'
##' Checked against socsim ground truth on the Bangladesh reporting networks: on
##' siblings, `vis_from_clique()` recovers the true visibility of **100%** of
##' alters. On maternal cousins --- which are not a clique, since cousinship is
##' not transitive --- it overstates visibility by 1.55x for off-frame alters
##' and 1.29x for on-frame ones. Because a death is always off-frame while
##' exposure is a mixture, that 1.20x *differential* does not cancel out of a
##' death rate; it biases it. Both cousin networks agree, so it is structural.
##'
##' @section The structures:
##'
##' \describe{
##'   \item{`"clique"`}{An equivalence relation: mutual and transitive, so it
##'     partitions the population into disjoint groups, and ego is a member of
##'     the group ego reports about. Siblings and household members.}
##'   \item{`"group"`}{Ego reports a group, but the relation is not an
##'     equivalence relation, so the groups overlap and ego's roster is not a
##'     clique. Cousins: your cousins need not be each other's cousins.}
##'   \item{`"star"`}{Ego reports alters who form no group with one another, and
##'     whose visibility must come from somewhere other than this roster.
##'     Parents, whose visibility is the number of their living children on the
##'     frame --- a fact about the *sibship*, not about the parent roster.}
##'   \item{`"unbounded"`}{No bounded group at all, so there is no roster from
##'     which visibility could be derived. Neighbours, acquaintances.}
##' }
##'
##' @section Properties of the tie, not of the rule:
##'
##' `ego.in.group` and `frame.indicator` are facts about the *tie*, so this is
##' where they belong. Both default to `NULL`, meaning "not declared", and a tie
##' that declares neither behaves exactly as before.
##'
##' Where a declared value meets one set somewhere else --- `ego.in.group` on
##' [vis_from_clique()], or `frame.indicator` passed to
##' [apply_visibility_rule()] --- **disagreement is an error, never a silent
##' precedence.** Quietly overriding one with the other would reintroduce the
##' failure this class exists to prevent: a number produced under an assumption
##' the caller did not know was in force. A value declared in only one place is
##' simply used.
##'
##' @param structure one of `"clique"`, `"group"`, `"star"`, `"unbounded"`; see
##'        Details. There is no default: that is the point.
##' @param name optional label for the tie, used in provenance output
##' @param ego.in.group is ego a member of the group ego reports about? `TRUE`
##'        for siblings and household members; `FALSE` for parents, whose
##'        visibility is a fact about the sibship rather than the parent roster.
##'        `NULL` (the default) leaves it undeclared, in which case the rule's
##'        own setting is used
##' @param frame.indicator name of the 0/1 column saying whether each alter is
##'        in the frame population **for this tie**. `NULL` (the default) leaves
##'        it undeclared. Worth setting once ties differ in who is eligible to
##'        report or be reported about --- neighbours bounded by a compound,
##'        say, against siblings who are not
##' @return an object of class `tie_config`
##' @examples
##'   tie_config("clique", name = "siblings")
##'   tie_config("group",  name = "maternal cousins")
##'   # parents: ego is not a member of the group whose size sets visibility
##'   tie_config("star", name = "parents", ego.in.group = FALSE)
##' @seealso [vis_from_clique()], [apply_visibility_rule()]
##' @export
##' @md
tie_config <- function(structure,
                       name            = NULL,
                       ego.in.group    = NULL,
                       frame.indicator = NULL) {

  structures <- c("clique", "group", "star", "unbounded")

  if (missing(structure)) {
    stop("tie_config() needs a structure, and deliberately has no default.\n",
         "One of: ", paste(structures, collapse = ", "), ".\n",
         "Siblings and household members are 'clique'; cousins are 'group' ",
         "(cousinship is not transitive); parents are 'star'; neighbours and ",
         "acquaintances are 'unbounded'.")
  }

  if (!is.character(structure) || length(structure) != 1 ||
      !structure %in% structures) {
    stop("structure must be one of: ", paste(structures, collapse = ", "),
         ". Got: ", paste(format(structure), collapse = ", "))
  }

  if (!is.null(name) && (!is.character(name) || length(name) != 1)) {
    stop("name must be a single string, or NULL.")
  }

  if (!is.null(ego.in.group) &&
      (!is.logical(ego.in.group) || length(ego.in.group) != 1 ||
       is.na(ego.in.group))) {
    stop("ego.in.group must be TRUE, FALSE, or NULL (undeclared).")
  }

  if (!is.null(frame.indicator) &&
      (!is.character(frame.indicator) || length(frame.indicator) != 1)) {
    stop("frame.indicator must be a single column name, or NULL (undeclared).")
  }

  ## Deliberately NOT enforced: that structure and ego.in.group "agree". It is
  ## tempting, since "clique" is described above as ego belonging to the group,
  ## but enforcing it removes real configurations. A household roster that
  ## excludes the respondent is a clique the respondent is outside of, and the
  ## Matlab rosters carry the respondent as a row, so their count is yprime.F
  ## rather than y.F. Both are legitimate, and the package's job here is to let
  ## them be stated, not to rule them out.

  structure(list(structure       = structure,
                 name            = name,
                 ego.in.group    = ego.in.group,
                 frame.indicator = frame.indicator),
            class = "tie_config")
}

##' @param x a `tie_config`
##' @param ... unused
##' @rdname tie_config
##' @export
print.tie_config <- function(x, ...) {
  cat("<tie_config>\n")
  cat("  structure:       ", x$structure, "\n", sep = "")
  if (!is.null(x$name))
    cat("  name:            ", x$name, "\n", sep = "")
  cat("  ego.in.group:    ",
      if (is.null(x$ego.in.group)) "(not declared; the rule's own setting is used)"
      else                          x$ego.in.group, "\n", sep = "")
  cat("  frame.indicator: ",
      if (is.null(x$frame.indicator)) "(not declared)" else x$frame.indicator,
      "\n", sep = "")
  invisible(x)
}

##' Is this a tie_config?
##' @param x any object
##' @return `TRUE` if `x` is a `tie_config`
##' @export
is_tie_config <- function(x) inherits(x, "tie_config")

##' Which rules may be applied to which ties
##'
##' Internal. `applies_to` on a rule is a character vector of structures, or
##' `NA_character_` meaning the rule makes no structural assumption and is valid
##' anywhere.
##'
##' @param rule a `visibility_rule`
##' @param tie a `tie_config`
##' @return `TRUE` if the rule may be applied to this tie
##' @keywords internal
rule_applies_to <- function(rule, tie) {
  aa <- rule$applies_to
  if (is.null(aa) || (length(aa) == 1 && is.na(aa))) return(TRUE)
  tie$structure %in% aa
}
