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
                            params = list()) {

  stopifnot(is.character(label), length(label) == 1)
  stopifnot(is.logical(is_estimated), length(is_estimated) == 1)
  stopifnot(is.function(fit), is.function(predict))

  structure(list(label        = label,
                 requires     = requires,
                 is_estimated = is_estimated,
                 fit          = fit,
                 predict      = predict,
                 assumptions  = assumptions,
                 params       = params),
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
    assumptions  = c(
      "the tie partitions the population into disjoint groups",
      if (isTRUE(ego.in.group)) "ego is a member of the group ego reports about",
      "reporting within the group is complete"),
    params       = list(ego.in.group = ego.in.group))
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
    requires     = ".sib.in.F",
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

  visibility_rule(
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
    params       = stats::setNames(rules, paste0("tier", seq_along(rules))))
}

## ---------------------------------------------------------------------------
## applying a rule
## ---------------------------------------------------------------------------

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
##' @param frame.indicator name of the 0/1 frame membership column
##' @param weights name of the column holding donor sampling weights
##' @param ego.in.group passed through when deriving `y.F`
##' @return a list with `values` (a tibble of `vis`, `vis_weight`, `vis_rule`,
##'         one row per row of `esc.dat`) and `provenance` (a `vis_provenance`
##'         tibble)
##' @export
apply_visibility_rule <- function(rule,
                                  esc.dat,
                                  ego.dat         = NULL,
                                  sib.dat         = NULL,
                                  ego.id          = ".ego.id",
                                  frame.indicator = ".sib.in.F",
                                  weights         = NULL,
                                  ego.in.group    = TRUE) {

  if (!is_visibility_rule(rule)) {
    stop("rule must be a visibility_rule, such as vis_from_clique(). Got: ",
         paste(class(rule), collapse = "/"))
  }

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
       provenance = vis_provenance(rule, values, esc.dat))
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
vis_provenance <- function(rule, values, esc.dat) {

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
