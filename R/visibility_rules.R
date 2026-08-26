## Visibility rules.
##
## A visibility rule says how to get, for each reported alter, the number of
## frame-population members who could have reported about that alter. The
## estimator divides each report by that number, so it is the quantity that
## turns "how many reports did we see" into "how big is the population".
##
## For siblings the rule is a theorem rather than a choice: `1/y.F` for an
## on-frame alter and `1/(y.F + 1)` otherwise. That follows from three facts
## about siblingship -- it partitions the population into disjoint groups, ego
## belongs to the group ego reports about, and reporting within the group is
## complete. Households satisfy all three. Cousins, parents and neighbours do
## not, and for those ties visibility generally is not identified from one-sided
## reports at all. The usual move is to substitute a summary of the
## *respondents'* visibility, which is a real modelling assumption with a
## knowable bias.
##
## This file makes both of those one interface, so an estimate can say which
## rule produced it and how much of it was approximated.

## ---------------------------------------------------------------------------
## the rule object
## ---------------------------------------------------------------------------

##' Construct a visibility rule
##'
##' A rule is a closure, not a table, and it is split into `fit` and `predict`.
##' The split is what lets one interface cover exact derivation, donor means and
##' (later) a fitted model, and what makes correct bootstrapping possible: a rule
##' whose `fit` step consumes the sample must be refit inside each bootstrap
##' replicate, and one whose does not must not be.
##'
##' @param label short string naming the rule; appears in provenance output
##' @param requires character vector of columns the rule needs, checked up front
##' @param is_estimated `TRUE` if `fit()` consumes the sample. Drives the
##'        bootstrap path: see [apply_visibility_rule()]
##' @param fit `function(donor.dat, weights)` returning opaque state
##' @param predict `function(alter.rows, state)` returning one row per row of
##'        `alter.rows`, with columns `vis`, `vis_weight` and `vis_rule`
##' @param assumptions character vector of assumptions this rule makes, carried
##'        into the provenance table so they reach output rather than living
##'        only in a methods appendix
##' @param params list of the constructor's arguments, for printing
##' @return an object of class `visibility_rule`
##' @keywords internal
visibility_rule <- function(label,
                            requires,
                            is_estimated,
                            fit,
                            predict,
                            assumptions = character(0),
                            params = list(),
                            applies_to = NA_character_,
                            tie_overridable = character(0),
                            declared = list(),
                            assumptions_fn = NULL) {

  stopifnot(is.character(label), length(label) == 1)
  stopifnot(is.logical(is_estimated), length(is_estimated) == 1)
  stopifnot(is.function(fit), is.function(predict))
  stopifnot(is.character(applies_to))

  structure(list(label        = label,
                 requires     = requires,
                 is_estimated = is_estimated,
                 fit          = fit,
                 predict      = predict,
                 assumptions  = assumptions,
                 params       = params,
                 ## which tie structures this rule is valid for; NA = any.
                 ## Checked by apply_visibility_rule() against a tie_config,
                 ## because applicability is not detectable from the data.
                 applies_to   = applies_to,
                 ## Names in the fit() state that a tie_config may set, because
                 ## they are properties of the tie rather than of the rule.
                 ## apply_visibility_rule() resolves these, and errors rather
                 ## than silently preferring one source over the other.
                 tie_overridable = tie_overridable,
                 ## Which of those the caller actually SET, as against leaving
                 ## at a default. Only a set value can disagree with a tie.
                 declared        = declared,
                 ## Assumptions that depend on a setting a tie may override
                 ## have to be computed from the FINAL state, not from the
                 ## constructor's arguments -- otherwise provenance can report
                 ## an assumption the rule did not actually make.
                 assumptions_fn  = assumptions_fn),
            class = "visibility_rule")
}

##' @export
print.visibility_rule <- function(x, ...) {
  cat("<visibility_rule: ", x$label, ">\n", sep = "")
  cat("  requires:     ", paste(x$requires, collapse = ", "), "\n", sep = "")
  cat("  is_estimated: ", x$is_estimated,
      if (x$is_estimated) "  (refit within each bootstrap replicate)"
      else                "  (fit once; frozen across bootstrap replicates)",
      "\n", sep = "")
  if (length(x$params)) {
    cat("  parameters:\n")
    for (nm in names(x$params)) {
      v <- x$params[[nm]]
      desc <- if (inherits(v, "visibility_rule")) paste0("<", v$label, ">")
              else if (is.null(v))                "NULL"
              else                                paste(format(v), collapse = ", ")
      cat("    ", nm, " = ", desc, "\n", sep = "")
    }
  }
  if (length(x$assumptions)) {
    cat("  assumptions:\n")
    for (a in x$assumptions) cat("    - ", a, "\n", sep = "")
  }
  invisible(x)
}

##' Is this object a visibility rule?
##'
##' @param x object to test
##' @return `TRUE` if `x` is a `visibility_rule`
##' @export
is_visibility_rule <- function(x) inherits(x, "visibility_rule")

## ---------------------------------------------------------------------------
## helpers
## ---------------------------------------------------------------------------

##' Weighted harmonic mean
##'
##' The individual estimator averages `1/v`, so the functional that makes a
##' plug-in visibility unbiased is `(E[1/v])^-1`, not `E[v]`. This is the
##' default summary for [vis_from_donor()].
##'
##' Restored from `get_visibility()`, which carried it as `wh.mean()` until the
##' ad-hoc adjustment factors were removed.
##'
##' @param x numeric vector; must be strictly positive
##' @param w numeric vector of weights, the same length as `x`
##' @return the weighted harmonic mean, `sum(w) / sum(w/x)`
##' @keywords internal
whmean <- function(x, w) {
  keep <- !is.na(x) & !is.na(w)
  x <- x[keep]; w <- w[keep]
  if (!length(x)) return(NA_real_)
  if (any(x <= 0)) {
    stop("whmean() needs strictly positive values; got ", sum(x <= 0),
         " value(s) <= 0. A visibility of zero would mean nobody could have ",
         "reported the alter, which cannot be true of an alter that was reported.")
  }
  sum(w) / sum(w / x)
}

##' Summarise donor visibilities
##'
##' @param x numeric vector of donor visibility counts
##' @param w numeric vector of donor weights
##' @param statistic one of `"harmonic"`, `"arithmetic"`, `"median"`
##' @return a single number
##' @keywords internal
vis_statistic <- function(x, w, statistic = c("harmonic", "arithmetic", "median")) {
  statistic <- match.arg(statistic)
  switch(statistic,
         harmonic   = whmean(x, w),
         arithmetic = stats::weighted.mean(x, w, na.rm = TRUE),
         ## an unweighted median; a weighted median would need a further
         ## dependency and the difference is immaterial at the cell sizes
         ## a donor rule is usable at
         median     = stats::median(x, na.rm = TRUE))
}

##' Normalise a `match_on` specification
##'
##' `match_on` describes the *alter*, but donors are respondents, and the two
##' frequently spell the same covariate differently (`.sib.sex` on an alter row,
##' `sex` on an ego row). A named vector expresses the correspondence:
##' `c(.sib.sex = "sex")` means "the alter column `.sib.sex` matches the donor
##' column `sex`". An unnamed entry means the column is spelled the same on both
##' sides.
##'
##' @param match_on character vector, optionally named
##' @return a named character vector, alter column -> donor column
##' @keywords internal
normalise_match_on <- function(match_on) {
  if (is.null(match_on) || !length(match_on)) return(character(0))
  nms <- names(match_on)
  if (is.null(nms)) nms <- rep("", length(match_on))
  nms[nms == ""] <- match_on[nms == ""]
  stats::setNames(as.character(match_on), nms)
}

## ---------------------------------------------------------------------------
## group information
## ---------------------------------------------------------------------------

##' Count group members on the frame, for each ego
##'
##' The generic form of `get_sibship_info()`. Counts, for each ego, how many of
##' the alters ego reports are in the frame population, and adds one for ego
##' when ego is a member of the group ego reports about.
##'
##' That `+ 1` is the whole content of `ego.in.group`. It is not about siblings:
##' it encodes "ego belongs to the group ego reports about", which is true of
##' siblings and of households, and false of parents and of neighbours.
##'
##' @param dat long-form alter data, one row per ego X alter
##' @param ego.id name of the column holding the ego id
##' @param frame.indicator name of the 0/1 column saying whether each alter is
##'        in the frame population
##' @param ego.in.group is ego a member of the group being reported about?
##' @return a tibble with one row per ego and columns `y.F` (alters on frame,
##'         not counting ego), `yprime.F` (on-frame group members including ego
##'         when `ego.in.group`) and `sib.size` (group size)
##' @keywords internal
get_group_info <- function(dat,
                           ego.id,
                           frame.indicator,
                           ego.in.group = TRUE) {

  ego.term <- as.integer(isTRUE(ego.in.group))

  dat <- dat %>% dplyr::rename(.ego.id   = !!sym(ego.id),
                               .sib.in.F = !!sym(frame.indicator))

  res <- dat %>%
    dplyr::group_by(.ego.id) %>%
    dplyr::summarize(y.F      = sum(.sib.in.F),
                     sib.size = dplyr::n() + ego.term,
                     .groups  = "drop") %>%
    dplyr::mutate(yprime.F = y.F + ego.term)

  res %>% dplyr::rename(!!ego.id := .ego.id)
}

## ---------------------------------------------------------------------------
## the clique rule
## ---------------------------------------------------------------------------

##' Assumptions the clique rule makes, given whether ego is in the group
##'
##' Internal. Computed from a value rather than fixed at construction, because a
##' [tie_config()] may override `ego.in.group` after the rule is built.
##'
##' @param ego.in.group is ego a member of the group ego reports about?
##' @return a character vector of assumptions
##' @keywords internal
clique_assumptions <- function(ego.in.group) {
  c("the tie partitions the population into disjoint groups",
    if (isTRUE(ego.in.group)) "ego is a member of the group ego reports about"
    else "ego is NOT a member of the group ego reports about, so ego is not counted",
    "reporting within the group is complete")
}

##' Visibility from a clique tie
##'
##' The exact rule, and the default everywhere. Reproduces
##' `calculate_sib_ind_visibility()` bit for bit: an alter who is on the frame
##' has visibility `y.F`, and an alter who is not has `y.F + 1`.
##'
##' This is a *theorem*, not a definition, and it is worth being precise about
##' what buys it. It follows when three things hold:
##'
##' * the tie partitions the population into disjoint groups (it is an
##'   equivalence relation, so it is transitive -- siblingship is, cousinship
##'   is not);
##' * ego is a member of the group ego reports about (`ego.in.group`);
##' * reporting within the group is complete.
##'
##' Given those, ego's own roster is sufficient to recover the visibility of
##' every alter, including alters whose neighbourhood ego cannot observe.
##' Siblings and households satisfy all three. Cousins, parents and neighbours
##' do not, and need [vis_from_donor()] or a rule yet to be written.
##'
##' The asymmetry between on-frame and off-frame alters is not a detail. A dead
##' alter is never on the frame, so every death is divided by `y.F + 1`, while
##' exposure is a mixture of both cases. That asymmetry is the only way
##' visibility survives into a rate, which is a ratio; a rule that assigned one
##' number to every alter in a cell would cancel out and reduce exactly to the
##' aggregate estimator.
##'
##' @param ego.in.group is ego a member of the group ego reports about? `TRUE`
##'        for siblings and households. Setting it `FALSE` drops the `+ 1`,
##'        which is what makes this the general clique rule rather than a
##'        sibling-specific one
##' @return a [visibility_rule]
##' @examples
##' rule <- vis_from_clique()
##' rule
##' @export
vis_from_clique <- function(ego.in.group = TRUE) {

  ## Whether the caller SET this, as against accepting the default, is what
  ## makes it possible to tell a real disagreement with a tie_config from a
  ## default quietly being overridden. Captured here because missing() only
  ## works in the frame the argument belongs to.
  ego.in.group.declared <- !missing(ego.in.group)

  label <- "clique"

  visibility_rule(
    label        = label,
    requires     = c("y.F", ".sib.in.F"),
    ## visibility is a function of ego's own reports, not of which egos happened
    ## to be sampled, so it is correct to freeze it across bootstrap replicates
    is_estimated = FALSE,
    fit          = function(donor.dat, weights) list(ego.in.group = ego.in.group),
    predict      = function(alter.rows, state) {
      ego.term <- as.integer(isTRUE(state$ego.in.group))
      ## vis = (on-frame members of the group) - (1 if this alter is one of them)
      ##     = (y.F + ego.term) - .sib.in.F
      ## which is y.F on frame and y.F + 1 off it, when ego.in.group is TRUE
      vis <- (alter.rows$y.F + ego.term) - alter.rows$.sib.in.F
      dplyr::tibble(vis        = as.numeric(vis),
                    vis_weight = 1 / vis,
                    vis_rule   = label)
    },
    assumptions  = clique_assumptions(ego.in.group),
    assumptions_fn = function(state) clique_assumptions(state$ego.in.group),
    params       = list(ego.in.group = ego.in.group),
    tie_overridable = "ego.in.group",
    declared     = list(ego.in.group = ego.in.group.declared),
    ## The 1/y.F vs 1/(y.F+1) rule is a theorem about cliques. On a tie that is
    ## not one it still returns a finite, plausible number -- silently wrong.
    ## See tie_config() for the socsim measurement of how wrong.
    applies_to   = "clique")
}

## ---------------------------------------------------------------------------
## the donor rule
## ---------------------------------------------------------------------------

##' Visibility borrowed from a donor population
##'
##' The approximation, and the first `is_estimated = TRUE` rule. Where
##' visibility cannot be derived from ego's own reports -- which is the normal
##' case for any tie that is not a clique -- borrow it from a donor population,
##' optionally matched on covariates.
##'
##' Read it as the simplest member of the predict-from-data family: `fit()` is a
##' grouped weighted mean where a model's would be a regression.
##'
##' @section What this assumes, and which way it is wrong:
##'
##' Donors are respondents: alive, and on the frame. A large share of the alters
##' needing an imputed visibility are dead. Wherever visibility correlates with
##' mortality -- through family size for kin ties, through living arrangements
##' for household ties -- the donor is systematically wrong, not merely noisy.
##' The direction is recorded in the rule's assumptions so that it reaches the
##' provenance table rather than staying in a methods appendix.
##'
##' @section Harmonic or arithmetic:
##'
##' The default is the weighted harmonic mean, because the individual estimator
##' averages `1/v`: the functional that makes the plug-in unbiased is
##' `(E[1/v])^-1`, not `E[v]`. `"arithmetic"` remains available, since it is
##' what the historical `y.F.bar / (y.F.bar + 1)` adjustment factor used. By
##' Jensen's inequality harmonic <= arithmetic, so the two disagree in a known
##' direction, by an amount that grows with the variance of visibility.
##'
##' @section Coverage failure is routine:
##'
##' `match_on` describes the alter, but donors are respondents. DHS interviews
##' women aged 15-49, so an alter aged 60 has no donor cell at all. Left alone
##' that surfaces as `NA` propagating silently into rates; `min_donors` together
##' with `on_missing = "error"` makes it loud instead. Use [vis_coalesce()] to
##' fall back to a coarser rule rather than to `NA`.
##'
##' @param donor `"egos"` to use the survey respondents, or a data frame of
##'        donors supplied directly
##' @param match_on covariates to match alters to donors on, or `NULL` for one
##'        global value. Names the alter's columns; where the donor frame spells
##'        a covariate differently, use a named vector, as in
##'        `c(.sib.sex = "sex")`
##' @param statistic `"harmonic"` (the default, and the right one for the
##'        individual estimator), `"arithmetic"` or `"median"`
##' @param donor_vis how the donors' *own* visibility is derived; a
##'        [visibility_rule], defaulting to [vis_from_clique()]
##' @param min_donors cells with fewer donors than this are treated as having no
##'        donors at all
##' @param on_missing what to do about an alter whose donor cell is missing or
##'        too small: `"error"`, `"fallback"` (use the global value) or `"na"`
##' @return a [visibility_rule]
##' @examples
##' vis_from_donor(match_on = c(.sib.sex = "sex"))
##' @export
vis_from_donor <- function(donor      = "egos",
                           match_on   = NULL,
                           statistic  = c("harmonic", "arithmetic", "median"),
                           donor_vis  = vis_from_clique(),
                           min_donors = 25,
                           on_missing = c("error", "fallback", "na")) {

  statistic  <- match.arg(statistic)
  on_missing <- match.arg(on_missing)
  match_map  <- normalise_match_on(match_on)

  if (!is_visibility_rule(donor_vis)) {
    stop("donor_vis must be a visibility_rule, such as vis_from_clique().")
  }

  label <- if (length(match_map)) {
             paste0("donor(", paste(names(match_map), collapse = "+"), ")")
           } else {
             "donor(global)"
           }

  visibility_rule(
    label        = label,
    ## the alter side needs the frame indicator to keep the on-frame /
    ## off-frame split; the group size itself comes from the donors
    ## The frame indicator, to keep the on-frame / off-frame split, plus the
    ## alter-side match_on columns: predict() joins on those, so a caller who
    ## matches on a covariate needs it carried through to the report rows. They
    ## belong in `requires` for the same reason anything else does -- it is what
    ## the up-front check reads, and what the estimator uses to decide which
    ## columns to keep.
    requires     = c(".sib.in.F", names(match_map)),
    ## S.hat is a sample quantity. Holding it fixed across bootstrap replicates
    ## would understate the variance, so this must be refit per replicate.
    is_estimated = TRUE,
    fit          = function(donor.dat, weights) {

      if (is.data.frame(donor)) donor.dat <- donor
      if (is.null(donor.dat)) {
        stop("vis_from_donor() has no donor data. Pass donor = <a data frame>, ",
             "or call apply_visibility_rule() with ego.dat so that ",
             "donor = 'egos' has something to use.")
      }

      ## Donors' own visibility. A donor is a frame member reporting about a
      ## group they belong to, so their group size is y.F + 1 -- the same
      ## quantity the clique rule would give an off-frame alter of theirs.
      if (!"y.F" %in% names(donor.dat)) {
        stop("donor data needs a 'y.F' column giving each donor's own count of ",
             "on-frame group members. donor data has: ",
             paste(names(donor.dat), collapse = ", "))
      }

      donor.dat <- donor.dat %>% dplyr::mutate(.donor.S = y.F + 1)

      w <- if (!is.null(weights) && weights %in% names(donor.dat)) {
             donor.dat[[weights]]
           } else {
             rep(1, nrow(donor.dat))
           }
      donor.dat$.donor.w <- w

      ## the global value, always computed: it is the fallback tier and it is
      ## also what match_on = NULL asks for
      global <- vis_statistic(donor.dat$.donor.S, donor.dat$.donor.w, statistic)

      cells <- NULL
      if (length(match_map)) {
        missing.donor.cols <- setdiff(unname(match_map), names(donor.dat))
        if (length(missing.donor.cols)) {
          stop("donor data is missing the column(s) needed to match on: ",
               paste(missing.donor.cols, collapse = ", "), ".\n",
               "donor data has: ", paste(names(donor.dat), collapse = ", "), "\n",
               "match_on names the ALTER's columns; where the donor frame ",
               "spells one differently, use a named vector, as in ",
               "match_on = c(.sib.sex = 'sex').")
        }
        cells <- donor.dat %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(unname(match_map)))) %>%
          dplyr::summarize(.S.hat    = vis_statistic(.donor.S, .donor.w, statistic),
                           n_donors  = dplyr::n(),
                           .groups   = "drop") %>%
          ## a cell backed by too few donors is worse than no cell: it looks
          ## like an answer. Blank it and let on_missing decide what happens.
          dplyr::mutate(.S.hat = ifelse(n_donors < min_donors, NA_real_, .S.hat))
        ## rename donor columns to the alter's spelling, so predict() can join
        ## on the alter's own column names
        names(cells)[match(unname(match_map), names(cells))] <- names(match_map)
      }

      list(global = global, cells = cells, match_map = match_map)
    },
    predict      = function(alter.rows, state) {

      n <- nrow(alter.rows)

      if (is.null(state$cells)) {
        S.hat    <- rep(state$global, n)
        n_donors <- rep(NA_integer_, n)
      } else {
        key <- names(state$match_map)
        missing.alter.cols <- setdiff(key, names(alter.rows))
        if (length(missing.alter.cols)) {
          stop("alter data is missing the column(s) named in match_on: ",
               paste(missing.alter.cols, collapse = ", "), ".\n",
               "alter data has: ", paste(names(alter.rows), collapse = ", "))
        }
        ## Join on EVERY match_on variable at once. Keying on a subset -- age
        ## alone, say -- silently duplicates every row once per level of the
        ## variable left out; doing the join here, in one place, retires that
        ## class of error for every caller.
        joined <- alter.rows %>%
          dplyr::select(dplyr::all_of(key)) %>%
          dplyr::left_join(state$cells, by = key)
        S.hat    <- joined$.S.hat
        n_donors <- joined$n_donors
      }

      unresolved <- is.na(S.hat)
      if (any(unresolved)) {
        if (on_missing == "error") {
          bad <- if (is.null(state$cells)) "(global)" else {
            u <- unique(alter.rows[unresolved, names(state$match_map), drop = FALSE])
            paste(utils::capture.output(print(utils::head(u, 10))), collapse = "\n")
          }
          stop(sum(unresolved), " of ", n, " alter row(s) have no usable donor ",
               "cell (missing, or fewer than ", min_donors, " donors).\n",
               "This is routine rather than exceptional: match_on describes the ",
               "alter, but donors are respondents, and a survey of women aged ",
               "15-49 has no donor for an alter aged 60.\n",
               "Use on_missing = 'fallback' to take the global value instead, ",
               "or wrap this rule in vis_coalesce() to fall through to a coarser ",
               "one. Unresolved cells:\n", bad)
        } else if (on_missing == "fallback") {
          S.hat[unresolved] <- state$global
        }
        ## on_missing == "na": leave them NA, which is what lets vis_coalesce()
        ## hand the row to the next rule
      }

      ## Preserve the on-frame / off-frame split. S.hat is a group size, so
      ## y.F.hat = S.hat - 1, and the clique arithmetic then gives S.hat - 1 for
      ## an on-frame alter and S.hat for an off-frame one. Collapsing this to a
      ## single number per cell would cancel out of the rate and reduce the
      ## estimator to the aggregate one.
      vis <- S.hat - alter.rows$.sib.in.F

      dplyr::tibble(vis        = as.numeric(vis),
                    vis_weight = 1 / vis,
                    vis_rule   = ifelse(is.na(vis), NA_character_,
                                        ifelse(unresolved, paste0(label, "/fallback"),
                                               label)),
                    n_donors   = n_donors)
    },
    assumptions  = c(
      paste0("visibility is borrowed from ",
             if (is.data.frame(donor)) "a supplied donor frame" else "the survey respondents",
             if (length(match_map)) paste0(", matched on ",
                                           paste(names(match_map), collapse = ", "))
             else ", with no matching"),
      paste0("donor visibilities are summarised by their weighted ", statistic, " mean"),
      "donors are alive and on the frame, but many alters needing an imputed visibility are dead; where visibility correlates with mortality the donor is systematically wrong, not merely noisy"),
    params       = list(donor      = if (is.data.frame(donor)) "<data frame>" else donor,
                        match_on   = if (length(match_map)) names(match_map) else NULL,
                        statistic  = statistic,
                        donor_vis  = donor_vis,
                        min_donors = min_donors,
                        on_missing = on_missing))
}

## ---------------------------------------------------------------------------
## visibility from a supplied group size
## ---------------------------------------------------------------------------

##' Visibility from a group size the caller supplies
##'
##' [vis_from_clique()] works out the group by counting ego's roster. Sometimes
##' the group whose size sets an alter's visibility is *not* the roster the
##' alter was reported on, and only the caller can say what it is. This rule
##' takes that count from a column instead of deriving it.
##'
##' @section When the group is not the roster:
##'
##' Three cases, all real:
##'
##' \describe{
##'   \item{Pooled ties}{Cousins are reported through the maternal and the
##'     paternal side separately, but an alter's visibility depends on everyone
##'     who could have reported them --- both sides, plus ego's own siblings.
##'     That pooled group is not any single roster.}
##'   \item{Nested ties}{"Pooled cousins, excluding ego's siblings" is the
##'     pooled group minus the sibship. A difference of two counts, which no
##'     roster holds directly.}
##'   \item{Borrowed groups}{A parent's visibility is the number of their
##'     children on the frame --- a fact about the *sibship*, not about the
##'     parent roster the parent was reported on. This is the mechanism the
##'     parents case needs.}
##' }
##'
##' @section What this rule does and does not assume:
##'
##' It makes **no structural assumption**, so it needs no `tie` and is valid for
##' any structure. That is not because it is safe in the way
##' [vis_from_donor()] is safe --- it is because the caller has taken
##' responsibility for the part [vis_from_clique()] would have derived. The
##' package can check the arithmetic; it cannot check that the column counts the
##' right people.
##'
##' What it still does for you is the frame split, which is easy to get wrong and
##' matters more than it looks. An alter who is themselves in the frame
##' population cannot report themselves, so their visibility is one lower than
##' the group total. Since a death is always off-frame while exposure is a
##' mixture, that asymmetry is the only route by which visibility reaches a
##' rate. Set `subtract.self = FALSE` only deliberately.
##'
##' @param size.var name of the column giving the number of frame-population
##'        members in the alter's group
##' @param counts.ego does `size.var` already count ego? Rosters that carry the
##'        respondent as a row do, in which case the column is the package's
##'        `yprime.F` rather than `y.F`. When `FALSE` and ego belongs to the
##'        group, one is added
##' @param subtract.self subtract one for an alter who is themselves in the
##'        frame population, since they cannot report themselves. `TRUE` unless
##'        you have a reason
##' @param ego.in.group is ego a member of the group being sized? Only consulted
##'        when `counts.ego` is `FALSE`. May be declared on the [tie_config()]
##'        instead
##' @param label optional short name for this basis, used in provenance. Worth
##'        setting when comparing several bases, so the output says which
##'        produced which estimate
##' @return a [visibility_rule]
##' @examples
##' # a roster that carries the respondent as a row, so the count includes ego
##' vis_from_group_size("n_in_cousinship_and_F")
##'
##' # the same, named for provenance
##' vis_from_group_size("n_in_pooled_and_F", label = "pooled cousins")
##' @seealso [vis_from_clique()], which derives the group instead of taking it
##' @export
##' @md
vis_from_group_size <- function(size.var,
                                counts.ego    = TRUE,
                                subtract.self = TRUE,
                                ego.in.group  = TRUE,
                                label         = NULL) {

  if (missing(size.var) || !is.character(size.var) || length(size.var) != 1) {
    stop("vis_from_group_size() needs size.var: the name of one column giving ",
         "the number of frame-population members in each alter's group.")
  }

  ego.in.group.declared <- !missing(ego.in.group)

  rule.label <- if (is.null(label)) paste0("group_size(", size.var, ")")
                else                label

  visibility_rule(
    label        = rule.label,
    requires     = c(size.var, ".sib.in.F"),
    ## The group size is read off the data, not estimated from the sample, so
    ## it is frozen across bootstrap replicates exactly as the clique rule is.
    ## If a caller computed the column FROM the sample, that is a dependence
    ## the package cannot see; say so in the assumptions below.
    is_estimated = FALSE,
    fit          = function(donor.dat, weights) list(ego.in.group = ego.in.group),
    predict      = function(alter.rows, state) {

      size <- alter.rows[[size.var]]

      ## add ego only when the column does not already count them
      ego.term <- if (isTRUE(counts.ego)) 0L
                  else as.integer(isTRUE(state$ego.in.group))

      ## an on-frame alter cannot report themselves
      self.term <- if (isTRUE(subtract.self)) alter.rows$.sib.in.F else 0

      vis <- size + ego.term - self.term

      dplyr::tibble(vis        = as.numeric(vis),
                    vis_weight = 1 / vis,
                    vis_rule   = rule.label)
    },
    assumptions  = c(
      paste0("visibility is taken from the supplied column '", size.var,
             "', which the caller has computed; the package does not check ",
             "that it counts the right people"),
      if (isTRUE(counts.ego)) "that column already counts ego"
      else if (isTRUE(ego.in.group)) "that column excludes ego, who is added back"
      else "that column excludes ego, who is not a member of the group",
      if (isTRUE(subtract.self))
        "an alter in the frame population does not count themselves"
      else
        "an alter in the frame population DOES count themselves, so the on-frame and off-frame visibilities are equal; note that a visibility with no frame split cancels out of a rate to the extent that it is constant within a cell"),
    params       = list(size.var      = size.var,
                        counts.ego    = counts.ego,
                        subtract.self = subtract.self,
                        ego.in.group  = ego.in.group,
                        label         = label),
    tie_overridable = "ego.in.group",
    declared     = list(ego.in.group = ego.in.group.declared))
}

## ---------------------------------------------------------------------------
## visibility from a fitted model
## ---------------------------------------------------------------------------

##' Visibility predicted from a fitted model
##'
##' The third member of the predict-from-data family. [vis_from_donor()] fits a
##' grouped weighted mean; this fits a model, and is otherwise the same idea:
##' learn how big a reporting group tends to be from donors whose group size is
##' known, then predict it for alters whose is not.
##'
##' A cell mean is a model with one categorical predictor and no pooling. This
##' buys three things over that: continuous predictors, several covariates
##' without the cell count collapsing, and borrowing strength across cells
##' rather than treating each in isolation --- which is what makes it usable
##' where `min_donors` would otherwise empty a cell.
##'
##' @section The formula is one-sided, and speaks the alter's vocabulary:
##'
##' Pass predictors only --- `~ age + sex`, not `S ~ age + sex`. The response is
##' always the donor's own group size, which the rule computes; naming it would
##' mean knowing an internal.
##'
##' Write the predictors as the **alter** rows spell them, since that is where
##' the model has to predict. Where the donor frame spells one differently, say
##' so with `predictors`, exactly as [vis_from_donor()]'s `match_on` does:
##' `predictors = c(.sib.sex = "sex")` reads "the alter column `.sib.sex` is the
##' donor column `sex`". The donor frame is renamed before fitting, so the
##' fitted object speaks one vocabulary throughout.
##'
##' @section Bootstrapping this is expensive, and has to be:
##'
##' A model with continuous predictors is not constant within an estimation
##' cell, so the cheap per-cell identity does not apply to it. The estimator
##' detects that and refits the model inside every bootstrap replicate, at
##' roughly M times the cost of a point estimate, warning as it goes. That is
##' the only correct route: holding a fitted model fixed across replicates would
##' treat an estimated quantity as known and understate the variance.
##'
##' @section What it does not check:
##'
##' That the model is any good. The package will tell you if a prediction is
##' impossible --- a non-positive group size --- but not if it is merely wrong.
##' Donors are alive and on the frame while many alters needing an imputed
##' visibility are dead, so a model fitted on donors extrapolates to a
##' population it never saw; that assumption is recorded in the provenance
##' rather than left implied.
##'
##' @param formula one-sided formula giving the predictors, in the alter's
##'        column names
##' @param predictors optional named vector mapping alter column names to donor
##'        column names, for predictors the two frames spell differently
##' @param family a `family` for `engine`. Defaults to `gaussian()`; a log link
##'        such as `poisson(link = "log")` is often the better choice, since it
##'        cannot predict a non-positive group size
##' @param engine the fitting function, taking `formula`, `data`, `family` and
##'        `weights`. Defaults to [stats::glm()]
##' @param donor `"egos"` to fit on the survey respondents, or a data frame
##' @param on_missing what to do about an alter the model cannot predict for:
##'        `"error"` or `"na"` (leave unresolved, so [vis_coalesce()] can try the
##'        next tier)
##' @param label optional short name for provenance
##' @return a [visibility_rule]
##' @examples
##' vis_from_model(~ .sib.sex, predictors = c(.sib.sex = "sex"))
##' @seealso [vis_from_donor()], the same idea with a cell mean in place of a model
##' @export
##' @md
vis_from_model <- function(formula,
                           predictors = NULL,
                           family     = stats::gaussian(),
                           engine     = stats::glm,
                           donor      = "egos",
                           on_missing = c("error", "na"),
                           label      = NULL) {

  if (missing(formula) || !inherits(formula, "formula")) {
    stop("vis_from_model() needs a one-sided formula giving the predictors, ",
         "such as ~ age + sex.")
  }
  if (length(formula) != 2) {
    stop("formula must be ONE-SIDED: ~ age + sex, not S ~ age + sex.\n",
         "The response is always the donor's own group size, which the rule ",
         "computes for you.")
  }

  on_missing <- match.arg(on_missing)
  pred.map   <- normalise_match_on(predictors)
  rhs.vars   <- all.vars(formula)

  rule.label <- if (is.null(label))
                  paste0("model(", paste(rhs.vars, collapse = "+"), ")")
                else label

  visibility_rule(
    label        = rule.label,
    requires     = c(".sib.in.F", rhs.vars),
    ## a fitted model is a sample quantity, so it must be refit per replicate
    is_estimated = TRUE,
    ## no structural assumption: the caller supplies the model, not a claim
    ## about how the tie is shaped
    applies_to   = NA_character_,
    fit          = function(donor.dat, weights) {

      if (is.data.frame(donor)) donor.dat <- donor
      if (is.null(donor.dat)) {
        stop("vis_from_model() has no donor data to fit on. Pass ",
             "donor = <a data frame>, or call apply_visibility_rule() with ",
             "ego.dat so that donor = 'egos' has something to use.")
      }
      if (!"y.F" %in% names(donor.dat)) {
        stop("donor data needs a 'y.F' column giving each donor's own count ",
             "of on-frame group members. donor data has: ",
             paste(names(donor.dat), collapse = ", "))
      }

      ## rename donor columns into the alter vocabulary the formula is written
      ## in, so the fitted object predicts on alter rows without translation
      if (length(pred.map)) {
        missing.donor <- setdiff(unname(pred.map), names(donor.dat))
        if (length(missing.donor)) {
          stop("donor data is missing the predictor column(s): ",
               paste(missing.donor, collapse = ", "), ".\n",
               "donor data has: ", paste(names(donor.dat), collapse = ", "))
        }
        for (i in seq_along(pred.map)) {
          donor.dat[[names(pred.map)[i]]] <- donor.dat[[unname(pred.map)[i]]]
        }
      }

      missing.pred <- setdiff(rhs.vars, names(donor.dat))
      if (length(missing.pred)) {
        stop("donor data is missing the predictor column(s) named in the ",
             "formula: ", paste(missing.pred, collapse = ", "), ".\n",
             "donor data has: ", paste(names(donor.dat), collapse = ", "), "\n",
             "The formula is written in the ALTER's column names; where the ",
             "donor frame spells one differently, map it with ",
             "predictors = c(alter_name = 'donor_name').")
      }

      ## the response: the donor's own group size
      donor.dat$.donor.S <- donor.dat$y.F + 1

      w <- if (!is.null(weights) && weights %in% names(donor.dat)) {
             donor.dat[[weights]]
           } else {
             rep(1, nrow(donor.dat))
           }

      full.formula <- stats::reformulate(
        termlabels = attr(stats::terms(formula), "term.labels"),
        response   = ".donor.S")

      fitted <- engine(full.formula, data = donor.dat, family = family,
                       weights = w)

      list(model = fitted)
    },
    predict      = function(alter.rows, state) {

      missing.cols <- setdiff(rhs.vars, names(alter.rows))
      if (length(missing.cols)) {
        stop("alter data is missing the predictor column(s): ",
             paste(missing.cols, collapse = ", "), ".\n",
             "alter data has: ", paste(names(alter.rows), collapse = ", "))
      }

      S.hat <- tryCatch(
        as.numeric(stats::predict(state$model, newdata = alter.rows,
                                  type = "response")),
        error = function(e) rep(NA_real_, nrow(alter.rows)))

      unresolved <- is.na(S.hat)
      if (any(unresolved) && on_missing == "error") {
        stop(sum(unresolved), " of ", nrow(alter.rows), " alter row(s) got no ",
             "prediction from the model -- usually a factor level the donors ",
             "never showed.\n",
             "Use on_missing = 'na', or wrap this rule in vis_coalesce() so a ",
             "coarser tier picks them up.")
      }

      ## A group size at or below zero is not a near miss, it is impossible:
      ## the alter was reported, so somebody could report them. Catch it here
      ## rather than letting a negative visibility become a negative weight.
      bad <- !is.na(S.hat) & S.hat <= 0
      if (any(bad)) {
        stop(sum(bad), " of ", nrow(alter.rows), " predicted group size(s) are ",
             "zero or negative, which cannot be: every alter here was reported ",
             "by somebody.\n",
             "This is what an identity link does when it extrapolates. Fit with ",
             "a log link -- family = poisson(link = 'log') -- which cannot ",
             "predict a non-positive value.")
      }

      vis <- S.hat - alter.rows$.sib.in.F

      dplyr::tibble(vis        = as.numeric(vis),
                    vis_weight = 1 / vis,
                    vis_rule   = ifelse(is.na(vis), NA_character_, rule.label))
    },
    assumptions  = c(
      paste0("visibility is predicted by a fitted model, ",
             paste(deparse(formula), collapse = " "),
             ", rather than derived"),
      paste0("fitted on ", if (is.data.frame(donor)) "a supplied donor frame"
                           else "the survey respondents"),
      "the model is fitted on donors, who are alive and on the frame, and then extrapolated to alters who are frequently neither; the package checks that a prediction is possible, not that it is right"),
    params       = list(formula    = paste(deparse(formula), collapse = " "),
                        predictors = if (length(pred.map)) names(pred.map) else NULL,
                        family     = if (is.character(family)) family else family$family,
                        donor      = if (is.data.frame(donor)) "<data frame>" else donor,
                        on_missing = on_missing,
                        label      = label))
}

## ---------------------------------------------------------------------------
## visibility as reported by ego
## ---------------------------------------------------------------------------

##' Visibility as reported by the respondent
##'
##' Ego is asked, about each alter, how many frame-population members that alter
##' is connected to. The answer is the alter's visibility, read straight off the
##' questionnaire instead of derived from a roster or borrowed from donors.
##'
##' @section Why this is the honest option for some ties:
##'
##' Every other rule here recovers visibility from something the survey happened
##' to collect for another purpose. [vis_from_clique()] exploits the tie's
##' structure, [vis_from_donor()] and [vis_from_model()] substitute the
##' respondents' own. All three are attempts to get at a quantity nobody was
##' asked about.
##'
##' For an `"unbounded"` tie there is no way round that. Neighbours and
##' acquaintances have no bounded group to count, so there is no roster to
##' derive from and nothing that makes the respondents a good stand-in for the
##' alters. Asking is the only route to the quantity, and this rule is what
##' turns the answer into an estimate.
##'
##' It is a **survey-design choice** as much as an analysis one: it costs
##' questionnaire time, and it can only be used if somebody decided in advance
##' to ask. Where the question was asked, though, it beats an approximation
##' derived from something else --- and unlike the other rules, it puts the
##' uncertainty somewhere visible, in reporting error rather than in an
##' assumption.
##'
##' @section Two things the answer may or may not include:
##'
##' The wording of the question decides both, and the package cannot tell from
##' the numbers which was meant.
##'
##' * `counts.ego` --- did the respondent count *themselves* among the alter's
##'   connections? "How many people like you does X know?" usually includes
##'   them; "how many *other* people like you" does not, and then one has to be
##'   added back.
##' * `counts.self` --- did the answer count the alter? A question phrased about
##'   a *group* ("how many people are in X's household?") does; one phrased about
##'   connections does not. When it does, this rule behaves like
##'   [vis_from_group_size()], which is the better constructor to reach for.
##'
##' @param report.var name of the column holding ego's reported count for each
##'        alter
##' @param counts.ego did the respondent count themselves? When `FALSE`, one is
##'        added back
##' @param counts.self did the answer count the alter themselves? When `TRUE`,
##'        one is subtracted for an alter who is in the frame population, since
##'        an alter cannot report themselves
##' @param on_missing what to do about an alter with no reported value:
##'        `"error"` or `"na"` (leave unresolved, so [vis_coalesce()] can try the
##'        next tier)
##' @param on_impossible what to do about a reported visibility below one:
##'        `"error"`, `"floor"` (raise it to one) or `"na"`. An alter who was
##'        reported was, by construction, visible to at least one person, so a
##'        zero is a data problem rather than a small number
##' @param label optional short name for provenance
##' @return a [visibility_rule]
##' @examples
##' vis_from_report("n_known_by")
##' vis_from_report("n_other_known_by", counts.ego = FALSE)
##' @seealso [vis_from_group_size()], for a reported *group size* rather than a
##'   reported degree
##' @export
##' @md
vis_from_report <- function(report.var,
                            counts.ego    = TRUE,
                            counts.self   = FALSE,
                            on_missing    = c("error", "na"),
                            on_impossible = c("error", "floor", "na"),
                            label         = NULL) {

  if (missing(report.var) || !is.character(report.var) ||
      length(report.var) != 1) {
    stop("vis_from_report() needs report.var: the name of one column holding ",
         "ego's reported count of how many frame-population members each alter ",
         "is connected to.")
  }

  on_missing    <- match.arg(on_missing)
  on_impossible <- match.arg(on_impossible)

  rule.label <- if (is.null(label)) paste0("reported(", report.var, ")")
                else label

  visibility_rule(
    label        = rule.label,
    requires     = c(report.var, ".sib.in.F"),
    ## read off the data, not fitted to the sample, so it is frozen across
    ## bootstrap replicates exactly as the clique rule is
    is_estimated = FALSE,
    ## no structural assumption whatever: this is the rule that works when the
    ## tie has no structure to exploit
    applies_to   = NA_character_,
    fit          = function(donor.dat, weights) list(),
    predict      = function(alter.rows, state) {

      reported <- as.numeric(alter.rows[[report.var]])

      ego.term  <- if (isTRUE(counts.ego)) 0 else 1
      self.term <- if (isTRUE(counts.self)) alter.rows$.sib.in.F else 0

      vis <- reported + ego.term - self.term

      missing.vis <- is.na(vis)
      if (any(missing.vis) && on_missing == "error") {
        stop(sum(missing.vis), " of ", nrow(alter.rows), " alter row(s) have no ",
             "reported visibility in '", report.var, "'.\n",
             "Item non-response on this question is normal, so this is usually ",
             "a reason to wrap the rule in vis_coalesce() with a fallback tier ",
             "rather than to stop. Use on_missing = 'na' to let it fall ",
             "through.")
      }

      ## An alter who was reported was visible to at least the person reporting
      ## them, so a visibility below one is not a small number -- it contradicts
      ## the existence of the report it sits on.
      impossible <- !is.na(vis) & vis < 1
      if (any(impossible)) {
        if (on_impossible == "error") {
          stop(sum(impossible), " of ", nrow(alter.rows), " reported ",
               "visibilities are below 1.\n",
               "That contradicts the report itself: this alter was named by a ",
               "respondent, so at least one frame member could see them. Common ",
               "causes are a don't-know code stored as 0, or counts.ego = TRUE ",
               "when the question actually excluded the respondent.\n",
               "Use on_impossible = 'floor' to raise them to 1, or 'na' to ",
               "leave them for another tier.")
        } else if (on_impossible == "floor") {
          vis[impossible] <- 1
        } else {
          vis[impossible] <- NA_real_
        }
      }

      dplyr::tibble(vis        = as.numeric(vis),
                    vis_weight = 1 / vis,
                    vis_rule   = ifelse(is.na(vis), NA_character_, rule.label))
    },
    assumptions  = c(
      paste0("visibility is taken from the respondent's reported count in '",
             report.var, "'"),
      if (isTRUE(counts.ego)) "the reported count includes the respondent"
      else "the reported count excludes the respondent, who is added back",
      if (isTRUE(counts.self))
        "the reported count includes the alter, who is subtracted when in the frame population"
      else "the reported count excludes the alter",
      "respondents report their alters' connections accurately; reporting error in this question passes straight into the estimate, where the other rules would instead carry an assumption"),
    params       = list(report.var    = report.var,
                        counts.ego    = counts.ego,
                        counts.self   = counts.self,
                        on_missing    = on_missing,
                        on_impossible = on_impossible,
                        label         = label))
}

## ---------------------------------------------------------------------------
## coalescing rules
## ---------------------------------------------------------------------------

##' Try visibility rules in priority order
##'
##' Takes rules in priority order. For each row, the first rule returning a
##' non-`NA` visibility wins, and the row records which tier resolved it.
##'
##' This is the mixed case -- some alters derivable exactly, some not -- and it
##' is what turns "38% of reported cousins were approximated, 4% of them from the
##' global mean" into a number the package reports rather than an assumption
##' nobody wrote down.
##'
##' Rules that would otherwise `stop()` on an unresolved row are run as though
##' `on_missing = "na"`, since falling through is the entire point here.
##'
##' @param ... two or more [visibility_rule] objects, most preferred first
##' @return a [visibility_rule]
##' @examples
##' vis_coalesce(vis_from_clique(),
##'              vis_from_donor(match_on = c(.sib.sex = "sex")),
##'              vis_from_donor(match_on = NULL))
##' @export
vis_coalesce <- function(...) {

  rules <- list(...)
  if (length(rules) < 2) {
    stop("vis_coalesce() needs at least two rules; got ", length(rules), ".")
  }
  if (!all(vapply(rules, is_visibility_rule, logical(1)))) {
    stop("every argument to vis_coalesce() must be a visibility_rule.")
  }

  label <- paste0("coalesce(", paste(vapply(rules, function(r) r$label, ""),
                                     collapse = " > "), ")")

  ## A chain is applicable wherever at least one of its tiers is. A tier with
  ## applies_to = NA is valid anywhere, so it makes the whole chain so.
  aa <- lapply(rules, function(r) r$applies_to)
  applies_to <- if (any(vapply(aa, function(a) length(a) == 1 && is.na(a), logical(1))))
                  NA_character_
                else unique(unlist(aa))

  out <- visibility_rule(
    label        = label,
    requires     = unique(unlist(lapply(rules, function(r) r$requires))),
    ## if any tier estimates from the sample, the whole chain does
    is_estimated = any(vapply(rules, function(r) r$is_estimated, logical(1))),
    fit          = function(donor.dat, weights) {
      lapply(rules, function(r) r$fit(donor.dat, weights))
    },
    predict      = function(alter.rows, state) {

      n   <- nrow(alter.rows)
      out <- dplyr::tibble(vis        = rep(NA_real_, n),
                           vis_weight = rep(NA_real_, n),
                           vis_rule   = rep(NA_character_, n),
                           vis_tier   = rep(NA_integer_, n))

      for (i in seq_along(rules)) {
        todo <- is.na(out$vis)
        if (!any(todo)) break

        ## An unresolved row is not an error here -- it is the signal to try the
        ## next tier -- so a rule that would stop() is asked not to.
        got <- tryCatch(rules[[i]]$predict(alter.rows[todo, , drop = FALSE],
                                           state[[i]]),
                        error = function(e) NULL)
        if (is.null(got)) next

        filled <- !is.na(got$vis)
        idx    <- which(todo)[filled]
        out$vis[idx]        <- got$vis[filled]
        out$vis_weight[idx] <- got$vis_weight[filled]
        out$vis_rule[idx]   <- got$vis_rule[filled]
        out$vis_tier[idx]   <- i
      }

      out
    },
    assumptions  = unlist(lapply(seq_along(rules), function(i) {
                     paste0("tier ", i, " (", rules[[i]]$label, "): ",
                            rules[[i]]$assumptions)
                   })),
    params       = stats::setNames(rules, paste0("tier", seq_along(rules))),
    applies_to   = applies_to)

  ## Keep the tiers reachable so that apply_visibility_rule() can drop the ones
  ## that do not apply to the declared tie. This is what makes a chain fall
  ## through for a non-clique tie: the clique tier is *removed* because it is
  ## inapplicable, not skipped because it happened to return NA. It never
  ## returns NA -- that was the bug.
  out$tiers <- rules
  out
}

##' Restrict a coalesced rule to the tiers valid for a declared tie
##'
##' Internal. Returns the rule unchanged when it has no tiers or all of them
##' apply; otherwise a rebuilt chain of the applicable ones, or the single
##' applicable rule. Attaches the dropped tier labels as an attribute so that
##' provenance can report them.
##'
##' @param rule a `visibility_rule`
##' @param tie a `tie_config`
##' @return a `visibility_rule`
##' @keywords internal
restrict_to_tie <- function(rule, tie) {

  if (is.null(rule$tiers) || is.null(tie)) return(rule)

  keep <- vapply(rule$tiers, rule_applies_to, logical(1), tie = tie)
  if (all(keep)) return(rule)

  dropped <- vapply(rule$tiers[!keep], function(r) r$label, "")

  if (!any(keep)) {
    stop("no tier of '", rule$label, "' applies to a tie declared as '",
         tie$structure, "'.
",
         "Dropped: ", paste(dropped, collapse = ", "), ".
",
         "Add a rule that makes no structural assumption, such as ",
         "vis_from_donor().")
  }

  kept <- rule$tiers[keep]
  out  <- if (length(kept) == 1) kept[[1]] else do.call(vis_coalesce, kept)
  attr(out, "dropped_tiers") <- dropped
  out
}

## ---------------------------------------------------------------------------
## applying a rule
## ---------------------------------------------------------------------------

##' Reconcile a value declared on a tie with one set on a rule or argument
##'
##' Internal. Returns the value to use, or stops when two explicitly-set values
##' disagree. Silent precedence is deliberately not offered: quietly preferring
##' one source would produce a number computed under an assumption the caller
##' did not know was in force, which is the failure this whole layer exists to
##' prevent.
##'
##' @param what name of the setting, for the message
##' @param tie.value value declared on the `tie_config`, or `NULL`
##' @param other.value value set elsewhere
##' @param other.declared was `other.value` actually set, or just a default?
##' @param other.where human-readable description of where `other.value` came from
##' @param default value to use when neither source declared one
##' @return the resolved value
##' @keywords internal
reconcile_tie_setting <- function(what, tie.value, other.value, other.declared,
                                  other.where, default) {

  tie.declared <- !is.null(tie.value)

  if (tie.declared && other.declared && !identical(tie.value, other.value)) {
    stop("conflicting values for '", what, "'.\n",
         "  tie_config() declares: ", format(tie.value), "\n",
         "  ", other.where, ": ", format(other.value), "\n\n",
         "These disagree, and neither silently wins: '", what, "' is a property ",
         "of the tie, so a rule set against it would compute under an ",
         "assumption you did not choose. Set it in one place, or set both to ",
         "the same value.")
  }

  if (tie.declared)   return(tie.value)
  if (other.declared) return(other.value)
  default
}

##' Apply a tie\'s declared settings to a fitted rule state
##'
##' Internal. A rule names, in `tie_overridable`, the state entries a tie may
##' set. Handles a coalesced rule by descending into each tier.
##'
##' @param rule the rule
##' @param state the state returned by the rule\'s `fit()`
##' @param tie a `tie_config`, or `NULL`
##' @return `state`, with tie-declared settings applied
##' @keywords internal
apply_tie_settings <- function(rule, state, tie) {

  if (is.null(tie)) return(state)

  ## a coalesced rule: state is one entry per tier
  if (!is.null(rule$tiers) && is.list(state) &&
      length(state) == length(rule$tiers)) {
    for (i in seq_along(rule$tiers)) {
      state[[i]] <- apply_tie_settings(rule$tiers[[i]], state[[i]], tie)
    }
    return(state)
  }

  for (nm in rule$tie_overridable) {
    tie.value <- tie[[nm]]
    if (is.null(tie.value)) next
    state[[nm]] <- reconcile_tie_setting(
      what           = nm,
      tie.value      = tie.value,
      other.value    = state[[nm]],
      other.declared = isTRUE(rule$declared[[nm]]),
      other.where    = paste0("visibility rule '", rule$label, "' sets"),
      default        = tie.value)
  }
  state
}

##' Apply a visibility rule to ego X alter X cell reports
##'
##' Validates the rule's `requires` against the columns actually present, fits
##' the rule, predicts a visibility for every row, and returns both the values
##' and a provenance table describing how they were arrived at.
##'
##' @param rule a [visibility_rule]
##' @param esc.dat ego X alter X cell reports, one row per report
##' @param ego.dat ego-level data, used as the donor frame when a rule asks for
##'        `donor = "egos"`. Needs a `y.F` column; if it has none and `sib.dat`
##'        is supplied, one is derived
##' @param sib.dat long-form alter data, used to derive `y.F` when `esc.dat` or
##'        `ego.dat` lacks it
##' @param ego.id name of the ego id column
##' @param frame.indicator name of the 0/1 frame membership column. `NULL` (the
##'        default) takes it from `tie`, falling back to `".sib.in.F"`. Setting
##'        it here as well as on the tie is an error if the two disagree
##' @param weights name of the column holding donor sampling weights
##' @param ego.in.group is ego a member of the group ego reports about? Governs
##'        how `y.F` is derived. `NULL` (the default) takes it from `tie`,
##'        falling back to `TRUE`. Setting it here as well as on the tie is an
##'        error if the two disagree; the tie is where it belongs
##' @param tie a [tie_config()] saying what kind of tie these reports are about.
##'        Required when `rule` assumes a tie structure --- [vis_from_clique()]
##'        does --- because applicability cannot be read off the data: on a tie
##'        that is not a clique the clique rule still returns a finite,
##'        plausible number, and it is wrong. A rule that makes no structural
##'        assumption, such as [vis_from_donor()], needs no `tie`. When `rule`
##'        is a [vis_coalesce()] chain, tiers inapplicable to `tie` are dropped,
##'        and named in the returned provenance.
##' @return a list with `values` (a tibble of `vis`, `vis_weight`, `vis_rule`,
##'         one row per row of `esc.dat`) and `provenance` (a `vis_provenance`
##'         tibble)
##' @export
apply_visibility_rule <- function(rule,
                                  esc.dat,
                                  ego.dat         = NULL,
                                  sib.dat         = NULL,
                                  ego.id          = ".ego.id",
                                  frame.indicator = NULL,
                                  weights         = NULL,
                                  ego.in.group    = NULL,
                                  tie             = NULL) {

  ## Both of these are properties of the TIE. They stay as arguments so that a
  ## caller with no tie_config can still set them, but a tie that declares one
  ## is authoritative, and a genuine disagreement is an error rather than a
  ## silent precedence. NULL means "not set here".
  frame.indicator.declared <- !is.null(frame.indicator)
  ego.in.group.declared    <- !is.null(ego.in.group)

  if (!is_visibility_rule(rule)) {
    stop("rule must be a visibility_rule, such as vis_from_clique(). Got: ",
         paste(class(rule), collapse = "/"))
  }

  ## ---- applicability -----------------------------------------------------
  ## A rule that assumes a tie structure may not be used until the caller has
  ## said what the structure is. There is no default, and no inference: given a
  ## roster of reports, a clique and a non-clique look identical, and
  ## vis_from_clique() returns a plausible number for both. See tie_config().
  restricted <- rule$applies_to
  restricted <- !(length(restricted) == 1 && is.na(restricted))

  if (!is.null(tie) && !is_tie_config(tie)) {
    stop("tie must be a tie_config(), or NULL. Got: ",
         paste(class(tie), collapse = "/"))
  }

  if (is.null(tie)) {
    if (restricted) {
      stop("visibility rule '", rule$label, "' is only valid for tie ",
           "structure(s): ", paste(rule$applies_to, collapse = ", "), ",
",
           "and no tie was declared. Pass tie = tie_config(\"...\").

",
           "This is deliberate. Applicability cannot be read off the data: on a ",
           "tie that is not a clique, vis_from_clique() still returns a finite, ",
           "plausible number, and it is wrong. Siblings and household members ",
           "are 'clique'; cousins are 'group'; parents are 'star'; neighbours ",
           "and acquaintances are 'unbounded'.")
    }
  } else if (!rule_applies_to(rule, tie)) {
    stop("visibility rule '", rule$label, "' is only valid for tie ",
         "structure(s): ", paste(rule$applies_to, collapse = ", "),
         ", but the tie was declared '", tie$structure, "'",
         if (!is.null(tie$name)) paste0(" (", tie$name, ")") else "", ".\n\n",
         "Applied anyway it would return a plausible, wrong number rather than ",
         "fail. Use a rule that makes no structural assumption -- ",
         "vis_from_donor() -- or vis_coalesce() them so the inapplicable tier ",
         "is dropped.")
  }

  ## ---- settings that belong to the tie -----------------------------------
  frame.indicator <- reconcile_tie_setting(
    what           = "frame.indicator",
    tie.value      = if (is.null(tie)) NULL else tie$frame.indicator,
    other.value    = frame.indicator,
    other.declared = frame.indicator.declared,
    other.where    = "apply_visibility_rule(frame.indicator =) was passed",
    default        = ".sib.in.F")

  ## ego.in.group is resolved twice over: once here, against this function's own
  ## argument, and again against the rule's setting inside apply_tie_settings()
  ## once the rule has been fitted. Both matter -- this one governs how y.F is
  ## derived, the other governs what the rule does with it, and before now
  ## nothing made the two agree.
  ego.in.group <- reconcile_tie_setting(
    what           = "ego.in.group",
    tie.value      = if (is.null(tie)) NULL else tie$ego.in.group,
    other.value    = ego.in.group,
    other.declared = ego.in.group.declared,
    other.where    = "apply_visibility_rule(ego.in.group =) was passed",
    default        = TRUE)

  ## Drop coalesce tiers that do not apply to this tie. This is what makes a
  ## chain fall through for a non-clique tie.
  rule <- restrict_to_tie(rule, tie)
  dropped.tiers <- attr(rule, "dropped_tiers")

  ## work on standard internal names, so a rule never has to know how the
  ## caller spells things
  if (!identical(ego.id, ".ego.id") && ego.id %in% names(esc.dat)) {
    esc.dat <- esc.dat %>% dplyr::mutate(.ego.id = !!sym(ego.id))
  }
  if (!identical(frame.indicator, ".sib.in.F") && frame.indicator %in% names(esc.dat)) {
    esc.dat <- esc.dat %>% dplyr::mutate(.sib.in.F = !!sym(frame.indicator))
  }

  ## Derive y.F whenever it is missing and can be had. Not only when the rule
  ## asks for it: an approximating rule needs no y.F of its own, but the
  ## downstream estimator still reads one off each report row, so a rule that
  ## did not require it would otherwise strip it out of the pipeline.
  if (!"y.F" %in% names(esc.dat)) {
    if (is.null(sib.dat)) {
      if ("y.F" %in% rule$requires) {
        stop("visibility rule '", rule$label, "' needs a 'y.F' column, and ",
             "esc.dat has none.\n",
             "esc.dat has: ", paste(names(esc.dat), collapse = ", "), "\n",
             "Either add y.F, or pass sib.dat so that it can be derived.")
      }
    } else {
    yF <- get_group_info(sib.dat %>% dplyr::mutate(.ego.id   = !!sym(ego.id),
                                                   .sib.in.F = !!sym(frame.indicator)),
                         ego.id          = ".ego.id",
                         frame.indicator = ".sib.in.F",
                         ego.in.group    = ego.in.group)
    esc.dat <- esc.dat %>%
      dplyr::left_join(yF %>% dplyr::select(.ego.id, y.F), by = ".ego.id")
    }
  }

  ## up-front column check, naming what is actually present -- the same shape
  ## as sibling_estimator()'s own check, which does this well
  missing.cols <- setdiff(rule$requires, names(esc.dat))
  if (length(missing.cols)) {
    stop("visibility rule '", rule$label, "' needs column(s) not present in the ",
         "report data: ", paste(missing.cols, collapse = ", "), ".\n",
         "The data has: ", paste(names(esc.dat), collapse = ", "))
  }

  ## the donor frame
  donor.dat <- ego.dat
  if (is.null(donor.dat) && !is.null(sib.dat)) {
    donor.dat <- get_group_info(sib.dat %>% dplyr::mutate(.ego.id   = !!sym(ego.id),
                                                          .sib.in.F = !!sym(frame.indicator)),
                                ego.id          = ".ego.id",
                                frame.indicator = ".sib.in.F",
                                ego.in.group    = ego.in.group)
    ## carry the donor covariates and weights across from the alter data
    extra <- setdiff(names(sib.dat), c(names(donor.dat), ".sib.in.F"))
    if (length(extra)) {
      first.rows <- sib.dat %>%
        dplyr::mutate(.ego.id = !!sym(ego.id)) %>%
        dplyr::group_by(.ego.id) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::select(dplyr::all_of(c(".ego.id", extra)))
      donor.dat <- donor.dat %>% dplyr::left_join(first.rows, by = ".ego.id")
    }
  } else if (!is.null(donor.dat) && !"y.F" %in% names(donor.dat) && !is.null(sib.dat)) {
    yF <- get_group_info(sib.dat %>% dplyr::mutate(.ego.id   = !!sym(ego.id),
                                                   .sib.in.F = !!sym(frame.indicator)),
                         ego.id          = ".ego.id",
                         frame.indicator = ".sib.in.F",
                         ego.in.group    = ego.in.group)
    donor.dat <- donor.dat %>%
      dplyr::mutate(.ego.id = !!sym(ego.id)) %>%
      dplyr::left_join(yF %>% dplyr::select(.ego.id, y.F), by = ".ego.id")
  }

  state  <- rule$fit(donor.dat, weights)
  ## A tie's declared settings reach the rule here, after fit and before
  ## predict, so that one declaration governs both how y.F was derived above
  ## and what the rule does with it.
  state  <- apply_tie_settings(rule, state, tie)
  ## Assumptions are recomputed from the final state, so provenance cannot
  ## report an assumption a tie declaration has since overridden.
  if (is.function(rule$assumptions_fn)) {
    rule$assumptions <- rule$assumptions_fn(state)
  }
  values <- rule$predict(esc.dat, state)

  if (nrow(values) != nrow(esc.dat)) {
    stop("visibility rule '", rule$label, "' returned ", nrow(values),
         " row(s) for ", nrow(esc.dat), " report(s). A rule's predict() must ",
         "return exactly one row per report.")
  }

  list(values     = values,
       ## the report data as the rule saw it, which is esc.dat plus any column
       ## derived on the way in (y.F, typically). Callers need this because the
       ## downstream estimator reads y.F off the report rows.
       data       = esc.dat,
       donor.dat  = donor.dat,
       state      = state,
       provenance = vis_provenance(rule, values, esc.dat,
                                   tie = tie, dropped.tiers = dropped.tiers,
                                   ego.in.group = ego.in.group,
                                   frame.indicator = frame.indicator))
}

##' Build a per-replicate refit function for an estimated visibility rule
##'
##' For a rule with `is_estimated = TRUE`, the estimated group size moves with
##' the bootstrap replicate. This returns the `refit` closure
##' [get_boot_ests_matrix()] expects: given a replicate index, it refits the
##' rule using that replicate's weights and returns the group size `S.hat` for
##' each row of `ec.dat`.
##'
##' Predicting with the frame indicator set to zero returns `S.hat` itself,
##' since a rule's visibility for an off-frame alter *is* the group size.
##'
##' @param rule the [visibility_rule]
##' @param donor.dat the donor frame
##' @param boot.weights data frame of bootstrap weights, with an ego id column
##'        and columns `boot_weight_1` ... `boot_weight_M`
##' @param ec.dat the ego X cell data the estimate is computed from
##' @param ego.id name of the ego id column
##' @return `function(r)` returning a numeric vector, one `S.hat` per row of
##'         `ec.dat`, or `NULL` if the rule does not estimate from the sample
##' @export
make_vis_refit <- function(rule, donor.dat, boot.weights, ec.dat, ego.id = ".ego.id") {

  if (!isTRUE(rule$is_estimated)) return(NULL)

  ## the rule reads S.hat off an off-frame prediction
  ec.for.pred <- ec.dat %>% dplyr::mutate(.sib.in.F = 0)

  function(r) {
    wcol <- paste0("boot_weight_", r)
    if (!wcol %in% names(boot.weights)) {
      stop("bootstrap weight column '", wcol, "' not found. An estimated ",
           "visibility rule needs the replicate weights to refit with.")
    }
    dd <- donor.dat %>%
      dplyr::left_join(boot.weights %>%
                         dplyr::select(dplyr::all_of(c(ego.id, wcol))),
                       by = ego.id)
    state <- rule$fit(dd, wcol)
    rule$predict(ec.for.pred, state)$vis
  }
}

##' Can a rule's visibility be predicted from ego X cell data?
##'
##' Internal. Decides between the two bootstrap paths for an estimated rule.
##' The cheap path recomputes visibility-adjusted sums from the frame-split
##' statistics in `ec.dat`, which is valid only when the rule's prediction is
##' constant within an ego X cell row --- equivalently, when everything the rule
##' needs is present in `ec.dat`.
##'
##' A rule matching on a covariate that is also a cell variable satisfies this,
##' which is the common case. One matching on something that cuts across cells,
##' or a model with continuous predictors, does not.
##'
##' @param rule a `visibility_rule`
##' @param ec.dat the ego X cell data
##' @return `TRUE` if the cheap per-cell path applies
##' @export
vis_is_cell_constant <- function(rule, ec.dat) {
  ## the frame indicator is supplied by the caller, not read from ec.dat
  needed <- setdiff(rule$requires, ".sib.in.F")
  ## a coalesced rule needs everything any of its tiers needs
  if (!is.null(rule$tiers)) {
    needed <- setdiff(unique(unlist(lapply(rule$tiers, function(r) r$requires))),
                      ".sib.in.F")
  }
  all(needed %in% names(ec.dat))
}

##' Build a per-replicate refit that recomputes visibility at the report level
##'
##' Internal. The expensive bootstrap path, for an estimated rule whose
##' visibility is *not* constant within an ego X cell row. There the frame-split
##' identity does not apply, and the only correct route is to refit the rule and
##' re-predict for every report inside each replicate, then re-aggregate.
##'
##' Costs roughly M times a point estimate. That is the price of not freezing a
##' sample quantity; falling back to the cheap path here would silently reinstate
##' the very bug the refit exists to prevent.
##'
##' @param rule the [visibility_rule]
##' @param donor.dat the donor frame
##' @param boot.weights data frame of replicate weights
##' @param esc.dat the ego X alter X cell reports, carrying `ind_vis`'s inputs
##' @param ec.dat the ego X cell data the estimate is computed from
##' @param cell.vars the columns defining a cell
##' @param ego.id name of the ego id column
##' @return `function(r)` returning a two-column data frame of `num` and `denom`,
##'         one row per row of `ec.dat`, or `NULL` if the rule is not estimated
##' @export
make_vis_refit_esc <- function(rule, donor.dat, boot.weights, esc.dat, ec.dat,
                               cell.vars, ego.id = ".ego.id") {

  if (!isTRUE(rule$is_estimated)) return(NULL)

  key <- c(ego.id, cell.vars)

  function(r) {
    wcol <- paste0("boot_weight_", r)
    if (!wcol %in% names(boot.weights)) {
      stop("bootstrap weight column '", wcol, "' not found.")
    }
    dd <- donor.dat %>%
      dplyr::left_join(boot.weights %>%
                         dplyr::select(dplyr::all_of(c(ego.id, wcol))),
                       by = ego.id)
    state <- rule$fit(dd, wcol)
    vw    <- rule$predict(esc.dat, state)$vis_weight

    sums <- esc.dat %>%
      dplyr::mutate(.vw = vw) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(key))) %>%
      dplyr::summarize(num   = sum(sib.occ * .vw, na.rm = TRUE),
                       denom = sum(sib.exp * .vw, na.rm = TRUE),
                       .groups = "drop")

    ## align to ec.dat row order, so the caller can index by row like the
    ## cheap path does
    out <- ec.dat %>%
      dplyr::select(dplyr::all_of(key)) %>%
      dplyr::left_join(sums, by = key)
    out$num[is.na(out$num)]     <- 0
    out$denom[is.na(out$denom)] <- 0
    out[, c("num", "denom")]
  }
}

## ---------------------------------------------------------------------------
## provenance
## ---------------------------------------------------------------------------

##' Summarise how a set of visibilities was arrived at
##'
##' @param rule the [visibility_rule] that produced `values`
##' @param values the tibble returned by the rule's `predict()`
##' @param esc.dat the report data the rule was applied to
##' @return a `vis_provenance` object
##' @keywords internal
vis_provenance <- function(rule, values, esc.dat,
                           tie = NULL, dropped.tiers = NULL,
                           ego.in.group = NA, frame.indicator = NA_character_) {

  n <- nrow(values)

  by_rule <- values %>%
    dplyr::mutate(.rule = ifelse(is.na(vis_rule), "(unresolved)", vis_rule)) %>%
    dplyr::group_by(.rule) %>%
    dplyr::summarize(n_alters = dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(share = n_alters / n) %>%
    dplyr::rename(rule = .rule)

  ## The share of deaths and the share of exposure that were approximated are
  ## different numbers, and both matter: an approximation that touches little
  ## exposure but most of the deaths is not a small approximation.
  approx <- !is.na(values$vis_rule) & values$vis_rule != "clique"
  occ <- if ("sib.occ" %in% names(esc.dat)) esc.dat$sib.occ else rep(NA_real_, n)
  exp <- if ("sib.exp" %in% names(esc.dat)) esc.dat$sib.exp else rep(NA_real_, n)

  share_of <- function(x) {
    if (all(is.na(x)) || sum(x, na.rm = TRUE) == 0) return(NA_real_)
    sum(x[approx], na.rm = TRUE) / sum(x, na.rm = TRUE)
  }

  structure(
    list(rule              = rule$label,
         is_estimated      = rule$is_estimated,
         ## what the caller declared the tie to be, and which tiers were
         ## dropped as inapplicable to it. Both belong in output: an estimate
         ## that silently discarded its exact tier should say so.
         tie               = if (is.null(tie)) NA_character_ else tie$structure,
         ## The settings as RESOLVED, after any tie declaration was applied.
         ## They change the arithmetic, so they belong in output rather than
         ## only in whichever call happened to set them.
         ego_in_group      = ego.in.group,
         frame_indicator   = frame.indicator,
         tie_name          = if (is.null(tie) || is.null(tie$name)) NA_character_
                             else tie$name,
         dropped_tiers     = dropped.tiers,
         n_alters          = n,
         by_rule           = by_rule,
         n_unresolved      = sum(is.na(values$vis)),
         share_approx      = mean(approx),
         share_deaths_approx   = share_of(occ),
         share_exposure_approx = share_of(exp),
         donor_cells       = if ("n_donors" %in% names(values)) {
                               values %>%
                                 dplyr::filter(!is.na(n_donors)) %>%
                                 dplyr::distinct(n_donors) %>%
                                 dplyr::arrange(n_donors)
                             } else NULL,
         assumptions       = rule$assumptions),
    class = "vis_provenance")
}

##' @export
print.vis_provenance <- function(x, ...) {
  cat("<vis_provenance>\n")
  cat("  rule:         ", x$rule, "\n", sep = "")
  cat("  is_estimated: ", x$is_estimated, "\n", sep = "")
  if (!is.na(x$tie)) {
    cat("  tie:          ", x$tie,
        if (!is.na(x$tie_name)) paste0(" (", x$tie_name, ")") else "",
        "\n", sep = "")
  }
  if (!is.null(x$ego_in_group) && !is.na(x$ego_in_group)) {
    cat("  ego.in.group: ", x$ego_in_group, "\n", sep = "")
  }
  if (!is.null(x$frame_indicator) && !is.na(x$frame_indicator) &&
      !identical(x$frame_indicator, ".sib.in.F")) {
    cat("  frame.indic.: ", x$frame_indicator, "\n", sep = "")
  }
  if (length(x$dropped_tiers)) {
    cat("  dropped:      ", paste(x$dropped_tiers, collapse = ", "),
        " -- inapplicable to this tie\n", sep = "")
  }
  cat("  alters:       ", x$n_alters, "\n", sep = "")
  cat("  resolved by:\n")
  for (i in seq_len(nrow(x$by_rule))) {
    cat(sprintf("    %-28s %8d  (%5.1f%%)\n",
                x$by_rule$rule[i], x$by_rule$n_alters[i], 100 * x$by_rule$share[i]))
  }
  if (x$n_unresolved > 0) {
    cat("  UNRESOLVED:   ", x$n_unresolved, "\n", sep = "")
  }
  pct <- function(v) if (is.na(v)) "n/a" else sprintf("%.1f%%", 100 * v)
  cat("  approximated: ", pct(x$share_approx), " of alters, ",
      pct(x$share_deaths_approx), " of deaths, ",
      pct(x$share_exposure_approx), " of exposure\n", sep = "")
  if (length(x$assumptions)) {
    cat("  assumptions:\n")
    for (a in x$assumptions) cat("    - ", a, "\n", sep = "")
  }
  invisible(x)
}
