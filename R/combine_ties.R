## Combining estimates across ties.
##
## A multi-tie survey asks the same respondents about several kinds of alter --
## siblings, household members, cousins, neighbours -- and each gives its own
## estimate of the same age-specific rates. There are three things one might
## want to do with them, and they are not variations of one operation:
##
##   compare  put them side by side on the same cells and look
##   pool     average them into one estimate
##   union    treat the ties as one larger tie and estimate once
##
## The first two combine RESULTS and live here. The third combines DATA -- it
## pools the reports and re-estimates -- so it cannot be done from finished
## estimates at all, and it needs something this package does not have. See
## `ties_union_check()` below.

## ---------------------------------------------------------------------------
## helpers
## ---------------------------------------------------------------------------

##' Pull the estimate table out of a set of estimator results
##'
##' Internal. Validates that the inputs look like estimator results, are named,
##' and share their cell columns.
##'
##' @param results a named list of results from [network_survival_estimator()]
##' @param estimator `"ind"` or `"agg"`
##' @param boot pull the replicate-level table instead of the summary
##' @return a named list of tibbles
##' @keywords internal
tie_estimate_tables <- function(results, estimator = c("ind", "agg"),
                                boot = FALSE) {

  estimator <- match.arg(estimator)
  slot <- paste0(if (boot) "boot.asdr." else "asdr.", estimator)

  if (!length(results)) {
    stop("no ties given. Pass estimator results as named arguments, as in ",
         "compare_ties(siblings = a, cousins = b).")
  }
  if (is.null(names(results)) || any(!nzchar(names(results)))) {
    stop("every tie must be named, since the name is what identifies it in the ",
         "output: compare_ties(siblings = a, cousins = b).")
  }

  missing.slot <- names(results)[vapply(results, function(r) is.null(r[[slot]]),
                                        logical(1))]
  if (length(missing.slot)) {
    stop("tie(s) ", paste(missing.slot, collapse = ", "), " have no '", slot,
         "' table.\n",
         if (boot)
           paste0("Replicate-level estimates come from ",
                  "network_survival_estimator(boot.weights = , return.boot = TRUE).")
         else
           "Each argument should be a result from network_survival_estimator().")
  }

  out <- lapply(results, function(r) r[[slot]])
  names(out) <- names(results)
  out
}

##' Which columns define a cell, given a set of estimate tables?
##'
##' Internal. The cell columns are whatever the tables share that is not an
##' estimated quantity.
##'
##' @param tabs a list of estimate tables
##' @return character vector of cell column names
##' @keywords internal
tie_cell_vars <- function(tabs) {

  computed <- c("num.hat", "denom.hat", "ind.y.F", "n", "wgt.sum", "asdr.hat",
                "estimator", "asdr.hat.ci.low", "asdr.hat.ci.high",
                "asdr.hat.median", "asdr.hat.se", "boot_idx", "event.name")

  cols <- lapply(tabs, function(x) setdiff(names(x), computed))
  common <- Reduce(intersect, cols)

  if (!length(common)) {
    stop("the ties share no cell columns, so their estimates cannot be lined ",
         "up.\n",
         "Columns by tie:\n",
         paste0("  ", names(tabs), ": ",
                vapply(cols, paste, "", collapse = ", "), collapse = "\n"), "\n",
         "Estimating each tie with the same cell_config() is what makes them ",
         "comparable.")
  }

  differing <- names(tabs)[vapply(cols, function(x) !setequal(x, common),
                                  logical(1))]
  if (length(differing)) {
    warning("tie(s) ", paste(differing, collapse = ", "), " have cell columns ",
            "the others do not; comparing on the shared ones only: ",
            paste(common, collapse = ", "), ".")
  }

  common
}

## ---------------------------------------------------------------------------
## compare
## ---------------------------------------------------------------------------

##' Put estimates from several ties side by side
##'
##' The simplest and least committal of the three combinations: line the ties up
##' on the same cells and look at them. Nothing is averaged and nothing is
##' assumed --- which is why it is worth doing first, and often worth doing
##' instead.
##'
##' Where two ties disagree by more than their intervals allow, that is
##' information: the ties differ in who they reach, in how completely they are
##' reported, or in how well their visibility rule holds. Averaging that away
##' before looking at it would be a mistake.
##'
##' @param ... estimator results, named by tie:
##'        `compare_ties(siblings = a, cousins = b)`
##' @param estimator `"ind"` (individual visibility) or `"agg"` (aggregate)
##' @return a `tie_comparison`: a tibble with one row per cell per tie, carrying
##'         each tie's estimate, interval and visibility provenance
##' @examples
##' \dontrun{
##' compare_ties(siblings = sib_est, cousins = cousin_est)
##' }
##' @seealso [pool_ties()] to average them, [ties_union_check()] for the third
##'   combination
##' @export
##' @md
compare_ties <- function(..., estimator = c("ind", "agg")) {

  estimator <- match.arg(estimator)
  results   <- list(...)
  tabs      <- tie_estimate_tables(results, estimator)
  cell.vars <- tie_cell_vars(tabs)

  keep <- c("asdr.hat", "num.hat", "denom.hat", "asdr.hat.ci.low",
            "asdr.hat.ci.high", "asdr.hat.se", "n")

  long <- dplyr::bind_rows(lapply(names(tabs), function(nm) {
    x <- tabs[[nm]]
    x <- x[, c(cell.vars, intersect(keep, names(x))), drop = FALSE]
    x$tie <- nm
    x
  }))

  long <- long %>% dplyr::relocate(dplyr::all_of("tie"))

  prov <- lapply(results, function(r) r$vis_provenance)

  structure(long,
            class     = c("tie_comparison", class(long)),
            cell.vars = cell.vars,
            estimator = estimator,
            provenance = prov)
}

##' @export
print.tie_comparison <- function(x, ...) {
  cat("<tie_comparison: ", attr(x, "estimator"), " estimator>\n", sep = "")
  cat("  cells: ", paste(attr(x, "cell.vars"), collapse = ", "), "\n", sep = "")

  prov <- attr(x, "provenance")
  if (length(prov)) {
    cat("  visibility by tie:\n")
    for (nm in names(prov)) {
      p <- prov[[nm]]
      cat(sprintf("    %-16s %-28s %s\n", nm,
                  if (is.null(p)) "(none recorded)" else p$rule,
                  if (is.null(p) || is.na(p$tie)) "" else paste0("[", p$tie, "]")))
    }
  }
  cat("\n")
  print(tibble::as_tibble(x), n = 12)
  invisible(x)
}

## ---------------------------------------------------------------------------
## pool
## ---------------------------------------------------------------------------

##' Average estimates from several ties into one
##'
##' Combines the ties' estimates cell by cell, as a weighted average.
##'
##' @section The independence problem, and how to avoid it:
##'
##' The obvious way to pool is inverse-variance weighting, which assumes the
##' estimates being combined are **independent**. In a multi-tie survey they are
##' not: every tie is reported by the *same respondents*, so a respondent
##' weighted up perturbs every tie at once.
##'
##' Where that correlation is positive --- which is what shared respondents
##' usually produce --- the independence formula gives an interval that is too
##' narrow. It is not guaranteed to err that way, though: with negatively
##' correlated ties it errs the other, and the size of the discrepancy is a
##' property of the data rather than something that can be reasoned out in
##' advance. The point is not the direction. It is that the formula is answering
##' a question about ties that do not exist.
##'
##' There is a way round it that costs nothing extra if the estimates were
##' bootstrapped with the **same replicate weights**. Pool within each replicate,
##' then take the spread across replicates: whatever correlation the ties have is
##' already in there, because each replicate perturbs all of them together. That
##' is `method = "replicate"`, and it is the default wherever the inputs allow
##' it.
##'
##' `method = "analytic"` is the inverse-variance formula, available for when
##' replicate estimates are not to hand. It warns, because the interval it
##' produces is not one you should quote without saying how it was made.
##'
##' @section Pooling is not the same as union:
##'
##' This treats each tie as a separate estimate of one quantity and averages
##' them. It does **not** add the ties' reports together --- that is union, it
##' double-counts any alter reachable through more than one tie, and it needs
##' information this package does not carry. See [ties_union_check()].
##'
##' @param ... estimator results, named by tie
##' @param estimator `"ind"` or `"agg"`
##' @param weights how to weight the ties: `"inverse-variance"` (the default),
##'        `"equal"`, or `"exposure"` (each tie's `denom.hat`)
##' @param method `"replicate"` to pool within bootstrap replicates, `"analytic"`
##'        for the inverse-variance formula, or `"auto"` (the default) to use
##'        replicates when every input has them
##' @return a `tie_pool`: a tibble with one row per cell
##' @examples
##' \dontrun{
##' pool_ties(siblings = sib_est, cousins = cousin_est)
##' }
##' @seealso [compare_ties()], which is usually worth doing first
##' @export
##' @md
pool_ties <- function(...,
                      estimator = c("ind", "agg"),
                      weights   = c("inverse-variance", "equal", "exposure"),
                      method    = c("auto", "replicate", "analytic")) {

  estimator <- match.arg(estimator)
  weights   <- match.arg(weights)
  method    <- match.arg(method)

  results <- list(...)
  tabs    <- tie_estimate_tables(results, estimator)
  cell.vars <- tie_cell_vars(tabs)

  have.boot <- all(vapply(results,
                          function(r) !is.null(r[[paste0("boot.asdr.", estimator)]]),
                          logical(1)))

  if (method == "auto") {
    method <- if (have.boot) "replicate" else "analytic"
  }
  if (method == "replicate" && !have.boot) {
    stop("method = 'replicate' needs replicate-level estimates for every tie, ",
         "from network_survival_estimator(boot.weights = , return.boot = TRUE).\n",
         "They must also come from the SAME replicate weights, since that is ",
         "what carries the correlation between ties.")
  }

  ## ---- the weight each tie gets, computed once from the point estimates ----
  w.tab <- dplyr::bind_rows(lapply(names(tabs), function(nm) {
    x <- tabs[[nm]]
    w <- switch(weights,
                "equal"            = rep(1, nrow(x)),
                "exposure"         = x$denom.hat,
                "inverse-variance" = {
                  if (!"asdr.hat.se" %in% names(x)) {
                    stop("weights = 'inverse-variance' needs a standard error ",
                         "for each tie, which comes from bootstrapping. Use ",
                         "weights = 'equal' or 'exposure', or estimate with ",
                         "boot.weights.")
                  }
                  ifelse(is.na(x$asdr.hat.se) | x$asdr.hat.se <= 0,
                         NA_real_, 1 / x$asdr.hat.se^2)
                })
    d <- x[, cell.vars, drop = FALSE]
    d$tie <- nm
    d$.w  <- w
    d
  }))

  if (method == "analytic") {
    warning("pooling analytically, which assumes the ties' estimates are ",
            "independent. They are not: every tie here is reported by the same ",
            "respondents, so a respondent weighted up perturbs all of them at ",
            "once.\n",
            "With positively correlated ties -- the usual result of shared ",
            "respondents -- this interval is too narrow. It can err the other ",
            "way too; how far, and in which direction, is a property of your ",
            "data.\n",
            "Estimate each tie with the same boot.weights and return.boot = ",
            "TRUE, then method = 'replicate' pools within replicates and ",
            "measures the correlation instead of assuming it away.")

    ## An interval needs a standard error from every tie. Without one the point
    ## estimate is still perfectly well defined, so pool it and say plainly that
    ## there is no uncertainty attached, rather than refusing outright.
    have.se <- all(vapply(tabs, function(x) "asdr.hat.se" %in% names(x),
                          logical(1)))

    est <- dplyr::bind_rows(lapply(names(tabs), function(nm) {
      cols <- c(cell.vars, "asdr.hat", if (have.se) "asdr.hat.se")
      x <- tabs[[nm]][, cols, drop = FALSE]
      x$tie <- nm
      x
    })) %>% dplyr::left_join(w.tab, by = c(cell.vars, "tie"))

    est <- est %>%
      dplyr::filter(!is.na(.data$.w), !is.na(.data$asdr.hat)) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(cell.vars)))

    out <- if (have.se) {
      est %>%
        dplyr::summarize(
          asdr.hat    = sum(.data$asdr.hat * .data$.w) / sum(.data$.w),
          ## the independence-assuming variance of a weighted mean
          asdr.hat.se = sqrt(sum((.data$.w / sum(.data$.w))^2 *
                                 .data$asdr.hat.se^2)),
          n_ties      = dplyr::n(),
          .groups     = "drop") %>%
        dplyr::mutate(
          asdr.hat.ci.low  = .data$asdr.hat - 1.96 * .data$asdr.hat.se,
          asdr.hat.ci.high = .data$asdr.hat + 1.96 * .data$asdr.hat.se)
    } else {
      message("no standard errors available, so the pooled estimate comes ",
              "without an interval. Estimate each tie with boot.weights to get ",
              "one.")
      est %>%
        dplyr::summarize(asdr.hat = sum(.data$asdr.hat * .data$.w) /
                                    sum(.data$.w),
                         n_ties   = dplyr::n(),
                         .groups  = "drop") %>%
        dplyr::mutate(asdr.hat.se      = NA_real_,
                      asdr.hat.ci.low  = NA_real_,
                      asdr.hat.ci.high = NA_real_)
    }

  } else {

    ## ---- pool inside each replicate ----------------------------------------
    boots <- tie_estimate_tables(results, estimator, boot = TRUE)

    reps <- vapply(boots, function(x) length(unique(x$boot_idx)), integer(1))
    if (length(unique(reps)) != 1) {
      stop("the ties have different numbers of bootstrap replicates (",
           paste(sprintf("%s: %d", names(reps), reps), collapse = ", "), ").\n",
           "Pooling within replicates only means anything if the replicates ",
           "correspond -- that is, if every tie was estimated from the same ",
           "boot.weights.")
    }

    long <- dplyr::bind_rows(lapply(names(boots), function(nm) {
      x <- boots[[nm]][, c(cell.vars, "boot_idx", "asdr.hat"), drop = FALSE]
      x$tie <- nm
      x
    })) %>% dplyr::left_join(w.tab, by = c(cell.vars, "tie"))

    per.rep <- long %>%
      dplyr::filter(!is.na(.data$.w), !is.na(.data$asdr.hat)) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(cell.vars, "boot_idx")))) %>%
      dplyr::summarize(pooled  = sum(.data$asdr.hat * .data$.w) / sum(.data$.w),
                       n_ties  = dplyr::n(),
                       .groups = "drop")

    ## the point estimate pools the point estimates; the replicates give its
    ## spread, correlation between ties included
    point <- dplyr::bind_rows(lapply(names(tabs), function(nm) {
      x <- tabs[[nm]][, c(cell.vars, "asdr.hat"), drop = FALSE]
      x$tie <- nm
      x
    })) %>%
      dplyr::left_join(w.tab, by = c(cell.vars, "tie")) %>%
      dplyr::filter(!is.na(.data$.w), !is.na(.data$asdr.hat)) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(cell.vars))) %>%
      dplyr::summarize(asdr.hat = sum(.data$asdr.hat * .data$.w) /
                                  sum(.data$.w),
                       n_ties   = dplyr::n(),
                       .groups  = "drop")

    spread <- per.rep %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(cell.vars))) %>%
      dplyr::summarize(
        asdr.hat.se      = stats::sd(.data$pooled, na.rm = TRUE),
        asdr.hat.ci.low  = stats::quantile(.data$pooled, 0.025, na.rm = TRUE),
        asdr.hat.ci.high = stats::quantile(.data$pooled, 0.975, na.rm = TRUE),
        .groups          = "drop")

    out <- point %>% dplyr::left_join(spread, by = cell.vars)
  }

  structure(out,
            class      = c("tie_pool", class(out)),
            cell.vars  = cell.vars,
            estimator  = estimator,
            weights    = weights,
            method     = method,
            ties       = names(tabs),
            provenance = lapply(results, function(r) r$vis_provenance))
}

##' @export
print.tie_pool <- function(x, ...) {
  cat("<tie_pool: ", attr(x, "estimator"), " estimator>\n", sep = "")
  cat("  ties:    ", paste(attr(x, "ties"), collapse = ", "), "\n", sep = "")
  cat("  weights: ", attr(x, "weights"), "\n", sep = "")
  cat("  method:  ", attr(x, "method"),
      if (identical(attr(x, "method"), "replicate"))
        "  (pooled within replicates, so correlation between ties is included)"
      else
        "  (assumes the ties are independent, which shared respondents make false)",
      "\n", sep = "")
  cat("\n")
  print(tibble::as_tibble(x), n = 12)
  invisible(x)
}

## ---------------------------------------------------------------------------
## union
## ---------------------------------------------------------------------------

##' Can these ties be unioned?
##'
##' Union is the third combination: treat several ties as one larger tie and
##' estimate once from their pooled reports. Unlike [compare_ties()] and
##' [pool_ties()] it cannot be done from finished estimates, because it changes
##' what is estimated rather than combining answers --- the reports have to be
##' pooled and the visibility recomputed on the union.
##'
##' It also needs something the package does not carry by default: **an identity
##' for alters that holds across ties.** An alter reachable through two ties must
##' be recognised as one alter, or their reports are counted twice and their
##' visibility computed as though two separate people. Alter ids in this package
##' are unique only within an ego, which is not enough.
##'
##' This function checks whether a supplied key does the job, and reports what
##' union would involve. It does not perform the union: once the key exists, the
##' operation is to bind the report rows, deduplicate on the key, and estimate
##' once with a `tie_config()` describing the union --- which is usually *not*
##' the structure of either part. Two cliques unioned are generally not a clique.
##'
##' @param ... report-level data frames, named by tie. Usually the `esc.dat`
##'        from each [network_survival_estimator()] result
##' @param alter.key name of a column identifying an alter *across* ties
##' @return a `ties_union_check`, invisibly; printed for its report
##' @examples
##' \dontrun{
##' ties_union_check(maternal = a$esc.dat, paternal = b$esc.dat,
##'                  alter.key = "person_id")
##' }
##' @seealso [pool_ties()], which averages estimates instead and needs no such key
##' @export
##' @md
ties_union_check <- function(..., alter.key = NULL) {

  parts <- list(...)
  if (length(parts) < 2) {
    stop("union needs at least two ties. Pass their report data named by tie.")
  }
  if (is.null(names(parts)) || any(!nzchar(names(parts)))) {
    stop("every tie must be named: ties_union_check(maternal = a, paternal = b).")
  }

  res <- list(ties = names(parts),
              n_reports = vapply(parts, nrow, integer(1)),
              alter.key = alter.key,
              usable = FALSE,
              overlap = NULL,
              reason = NULL)

  if (is.null(alter.key)) {
    res$reason <- paste0(
      "no alter.key given. Union needs an identity for alters that holds ",
      "across ties; ids in this package are unique only within an ego.")
    return(structure(res, class = "ties_union_check"))
  }

  missing.key <- names(parts)[vapply(parts,
                                     function(x) !alter.key %in% names(x),
                                     logical(1))]
  if (length(missing.key)) {
    res$reason <- paste0("tie(s) ", paste(missing.key, collapse = ", "),
                         " have no column '", alter.key, "'.")
    return(structure(res, class = "ties_union_check"))
  }

  keys <- lapply(parts, function(x) unique(as.character(x[[alter.key]])))
  all.keys <- unique(unlist(keys))

  ## how many alters appear in more than one tie?
  in.how.many <- rowSums(vapply(keys, function(k) all.keys %in% k,
                                logical(length(all.keys))))
  shared <- sum(in.how.many > 1)

  res$overlap <- list(n_alters = length(all.keys),
                      n_shared = shared,
                      share    = if (length(all.keys)) shared / length(all.keys)
                                 else NA_real_)
  res$usable <- TRUE
  res$reason <- if (shared == 0) {
    "the ties' alter sets are disjoint, so pooling their reports double-counts nobody."
  } else {
    paste0(shared, " alter(s) appear in more than one tie. Their reports must be ",
           "deduplicated on the key, and their visibility recomputed on the ",
           "union rather than added: a frame member who could report them ",
           "through both ties is one reporter, not two.")
  }

  structure(res, class = "ties_union_check")
}

##' @export
print.ties_union_check <- function(x, ...) {
  cat("<ties_union_check>\n")
  cat("  ties:      ", paste(x$ties, collapse = ", "), "\n", sep = "")
  cat("  reports:   ", paste(sprintf("%s=%d", x$ties, x$n_reports),
                             collapse = ", "), "\n", sep = "")
  cat("  alter.key: ",
      if (is.null(x$alter.key)) "(none given)" else x$alter.key, "\n", sep = "")

  if (!is.null(x$overlap)) {
    cat(sprintf("  alters:     %d distinct, %d in more than one tie (%.1f%%)\n",
                x$overlap$n_alters, x$overlap$n_shared,
                100 * x$overlap$share))
  }
  cat("  usable:    ", x$usable, "\n", sep = "")
  cat("  ", x$reason, "\n", sep = "")

  if (x$usable) {
    cat("\n  To union: bind the report rows, deduplicate on the key, and\n")
    cat("  estimate once with a tie_config() describing the UNION. That is\n")
    cat("  usually not the structure of either part -- two cliques unioned\n")
    cat("  are generally not a clique.\n")
  }
  invisible(x)
}
