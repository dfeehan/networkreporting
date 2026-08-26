##' Estimate death rates from network reporting data
##'
##' The generic network survival estimator. Given reports about alters connected
##' to survey respondents by some tie, it produces age-specific rates using both
##' the individual-visibility and the aggregate-visibility estimators.
##'
##' [siblingsurvival::sibling_estimator()] is this function with the sibling
##' names and the clique tie filled in; if you are working with sibling
##' histories, use that.
##'
##' @section The tie is required:
##'
##' There is no default `tie`, and that is the point. Which kind of tie a set of
##' reports is about cannot be read off the data: on a tie that is not a clique,
##' the default [vis_from_clique()] rule still returns a finite, plausible
##' number, and it is wrong --- overstating visibility by around 1.55x for
##' off-frame alters against 1.29x for on-frame ones, measured against socsim
##' ground truth on cousins. Because a death is always off-frame while exposure
##' is a mixture, that differential biases the rate rather than cancelling out of
##' it. See [tie_config()].
##'
##' @param rel.dat The long-form ego X alter dataset: one row per reported alter,
##'        per ego
##' @param ego.id String naming the column of \code{rel.dat} with the survey
##'        respondent's id
##' @param alter.id String naming the column of \code{rel.dat} with the alter's id
##' @param frame.indicator String naming the 0/1 column of \code{rel.dat} saying
##'        whether each alter is in the frame population
##' @param alter.sex String naming the alter attribute that enters the estimation
##'        cells alongside age and time period. Called `alter.sex` because sex is
##'        what it is in every current application; carrying several such
##'        attributes, rather than one plus \code{cell.config$covars}, is future
##'        work
##' @param cell.config An object from [cell_config()] configuring the cells
##' @param weights String naming the column of \code{rel.dat} with the sampling
##'        weight
##' @param boot.weights Optional dataframe of bootstrap resampled weights; see
##'        Details
##' @param return.boot If TRUE, and \code{boot.weights} is given, return every
##'        bootstrap estimate rather than only their summaries
##' @param visibility A [visibility_rule] saying how each alter's visibility is
##'        derived. Defaults to [vis_from_clique()], which is exact for a clique
##'        tie and refuses any other structure
##' @param tie A [tie_config()] saying what kind of tie these reports are about.
##'        **Required**; see above
##' @param discretize.exp Boolean for whether exposure should be discretized. Not
##'        yet implemented
##' @param .arg.labels Internal. Named character vector letting a wrapper phrase
##'        the up-front column-check message in its own argument names
##' @param .data.label Internal. Name to use for the data argument in that message
##' @return a list with \code{asdr.ind} (individual-visibility estimates),
##'         \code{asdr.agg} (aggregate-visibility estimates), \code{ec.dat},
##'         \code{esc.dat}, and a \code{vis_provenance} object saying how
##'         visibility was arrived at
##'
##' @section Details:
##' If you want estimated sampling variances, pass a data frame
##' \code{boot.weights}. It is assumed to have a column named whatever
##' \code{ego.id} is, and then columns \code{boot_weight_1}, ...,
##' \code{boot_weight_M}.
##'
##' @seealso [siblingsurvival::sibling_estimator()], [tie_config()],
##'   [vis_from_clique()], [vis_from_donor()]
##' @export
##' @md
network_survival_estimator <- function(rel.dat,
                                      # name of the ego id column
                                      ego.id,
                                      # name of the alter id column
                                      alter.id,
                                      # name of the 0/1 column saying whether each
                                      # alter is in the frame population
                                      frame.indicator,
                                      # alter attribute that enters the estimation
                                      # cells alongside age and time period
                                      alter.sex = 'sex',
                                      cell.config,
                                      weights,
                                      boot.weights = NULL,
                                      return.boot = FALSE,
                                      # how each reported alter's visibility is derived
                                      visibility = vis_from_clique(),
                                      # what kind of tie these reports are about.
                                      # NO default: applicability cannot be read off
                                      # the data, so the caller has to say. See
                                      # tie_config().
                                      tie,
                                      # by default, we report continuous exposure (ie, number of months of exposure)
                                      # but the formal results are based on exposed/not exposed; use this setting to
                                      # discretize exposure
                                      discretize.exp=FALSE,
                                      # internal: let a wrapper phrase the
                                      # up-front column-check message in its own
                                      # argument names rather than these
                                      .arg.labels = character(0),
                                      .data.label = "rel.dat") {

  if (missing(tie)) {
    stop("network_survival_estimator() needs a tie, and deliberately has no ",
         "default.\n",
         "Which kind of tie a set of reports is about cannot be read off the ",
         "data: on a tie that is not a clique, the default clique visibility ",
         "rule still returns a finite, plausible number, and it is wrong.\n",
         "Siblings and household members are tie_config('clique'); cousins are ",
         "'group'; parents are 'star'; neighbours and acquaintances are ",
         "'unbounded'.\n",
         "If you are estimating from sibling histories, siblingsurvival::",
         "sibling_estimator() supplies the clique tie for you.")
  }

  ## check up front that the columns we were given actually exist, so that a
  ## mismatched name (eg alter.id='alter.id' when the data has 'alterid') produces a
  ## message that names the columns available rather than an opaque tidyselect error
  ##
  ## The labels come from .arg.labels so that a wrapper -- siblingsurvival's
  ## sibling_estimator() -- can have the message name ITS arguments rather than
  ## this function's. Telling someone who wrote sib.id = that alter.id is wrong
  ## sends them looking for an argument they never used.
  requested.cols <- c(ego.id=ego.id,
                      alter.id=alter.id,
                      frame.indicator=frame.indicator,
                      alter.sex=alter.sex,
                      weights=weights)
  names(requested.cols) <- ifelse(names(requested.cols) %in% names(.arg.labels),
                                  .arg.labels[names(requested.cols)],
                                  names(requested.cols))
  missing.cols <- requested.cols[! requested.cols %in% names(rel.dat)]

  if (length(missing.cols) > 0) {
    stop(glue::glue(
      "Column(s) requested but not found in {.data.label}: ",
      "{paste0(names(missing.cols), \"='\", missing.cols, \"'\", collapse=', ')}.\n",
      "{.data.label} has columns: {paste0(names(rel.dat), collapse=', ')}\n"))
  }

  rel.dat <- rel.dat %>%
    dplyr::mutate(.ego.id     = !!sym(ego.id),
                  .sib.id     = !!sym(alter.id),
                  .sib.in.F   = !!sym(frame.indicator),
                  .sib.sex    = !!sym(alter.sex),
                  .ego.weight = !!sym(weights))

  # get ego X alter X cell reports
  esc.dat <- get_esc_reports(sib.dat=rel.dat,
                             ego.id='.ego.id',
                             sib.id='.sib.id',
                             cell.config)

  # add covariates for the alters
  #
  # A visibility rule may need columns beyond these -- vis_from_group_size()
  # needs whichever column holds the group size -- so carry anything the rule
  # declares in `requires` and that rel.dat actually has. Without this a rule
  # can be applied through apply_visibility_rule() but not through the
  # estimator, which is the wrong way round.
  rule.cols <- setdiff(visibility$requires, c('.sib.in.F', 'y.F'))
  rule.cols <- intersect(rule.cols, names(rel.dat))

  esc.dat <- esc.dat %>%
    left_join(rel.dat %>% select(.ego.id,
                                 .sib.id,
                                 .ego.weight,
                                 .sib.in.F,
                                 .sib.sex,
                                 all_of(rule.cols)),
              by=c('.ego.id', '.sib.id'))

  cell.vars <- c('time.period', '.sib.sex', 'agelabel', cell.config$covars)

  ## Apply the visibility rule. The default, vis_from_clique(), reproduces the
  ## previous hardcoded behaviour exactly -- 1/y.F on frame, 1/(y.F + 1) off it
  ## -- so nothing about existing estimates moves. Passing another rule is what
  ## A tie may declare its own frame.indicator, naming a column in the CALLER's
  ## data. By this point that column has been renamed to .sib.in.F, so the two
  ## are reconciled here rather than downstream, where the tie's name would no
  ## longer be found. Checking against frame.indicator is the comparison
  ## that means anything at this level.
  if (!is.null(tie$frame.indicator) &&
      !identical(tie$frame.indicator, frame.indicator)) {
    stop(glue::glue(
      "conflicting frame indicators.\n",
      "  tie_config() declares:            '{tie$frame.indicator}'\n",
      "  network_survival_estimator(frame.indicator =) got: '{frame.indicator}'\n\n",
      "Both name the column saying whether an alter is in the frame ",
      "population, and they disagree. Set one of them, or set both the same."))
  }
  if (!is.null(tie$frame.indicator)) {
    ## re-point it at the internal name the rename produced
    tie <- tie_config(structure       = tie$structure,
                                        name            = tie$name,
                                        ego.in.group    = tie$ego.in.group,
                                        frame.indicator = '.sib.in.F')
  }

  ## makes visibility a declared modelling choice rather than an assumption
  ## buried in the estimator.
  vis.res <- apply_visibility_rule(
    rule            = visibility,
    esc.dat         = esc.dat,
    sib.dat         = rel.dat,
    ego.id          = '.ego.id',
    frame.indicator = '.sib.in.F',
    weights         = '.ego.weight',
    tie             = tie)

  ## esc.dat comes back with y.F attached, which get_ec_reports() reads
  esc.dat <- vis.res$data
  ## `ind_vis` is the visibility WEIGHT (the reciprocal of the count), which is
  ## what get_ec_reports() consumes
  esc.dat$ind_vis <- vis.res$values$vis_weight

  if (any(is.na(esc.dat$ind_vis))) {
    n.na <- sum(is.na(esc.dat$ind_vis))
    stop(glue::glue(
      "The visibility rule '{visibility$label}' left {n.na} of {nrow(esc.dat)} ",
      "report(s) without a visibility.\n",
      "For the clique rule this points at missingness in the frame indicator. ",
      "For an approximating rule it usually means some alters have no donor ",
      "cell; wrap the rule in vis_coalesce() with a coarser fallback tier."))
  }

  ## TODO - I think this line sometimes causes a warning
  ## "Column `.ego.id` has different attributes on LHS and RHS of join"
  ec.dat <- get_ec_reports(esc.dat,
                           ego.id='.ego.id',
                           sib.dat=rel.dat,
                           sib.frame.indicator='.sib.in.F',
                           # TODO - eventually, perhaps these should be
                           # parameters and not hard-coded
                           cell.vars=cell.vars,
                           weights='.ego.weight',
                           ind.vis.var='ind_vis')

  asdr.ind.dat <- get_ind_est_from_ec(ec.dat, '.ego.weight', cell.vars)
  asdr.agg.dat <- get_agg_est_from_ec(ec.dat, '.ego.weight', cell.vars)

  ## if we want sampling variances...
  if (! is.null(boot.weights)) {
    M <- ncol(boot.weights) - 1

    boot.weights <- boot.weights %>%
      dplyr::rename(.ego.id = !!sym(ego.id))

    ## For an estimated visibility rule, the group size moves with the
    ## replicate, so it has to be refit inside the loop rather than frozen.
    ## For vis_from_clique() this is NULL and nothing changes -- which is what
    ## makes the change safe to land: the clique CIs must not move.
    ## Two paths, and which one applies is a fact about the rule rather than a
    ## preference. If everything the rule needs is present in ec.dat, its
    ## visibility is constant within an ego X cell row and the frame-split
    ## identity recovers the replicate estimate cheaply. If not -- a rule
    ## matching on something that cuts across cells, or a model with continuous
    ## predictors -- that identity does not hold, and the only correct route is
    ## to refit and re-predict for every report inside each replicate.
    ##
    ## Falling back to the cheap path in the second case would freeze what the
    ## rule estimates and reinstate exactly the bug is_estimated exists to
    ## prevent, so it is not offered.
    vis.refit      <- NULL
    vis.refit.sums <- NULL

    if (isTRUE(visibility$is_estimated) &&
        !vis_is_cell_constant(visibility, ec.dat)) {

      warning(glue::glue(
        "visibility rule '{visibility$label}' is estimated from the sample and ",
        "its prediction is not constant within a cell, so it has to be refit ",
        "and re-predicted for every report inside each of the {M} bootstrap ",
        "replicates.\n",
        "That costs roughly {M} times a point estimate. It is the only correct ",
        "route: reusing the per-cell shortcut would hold a sample quantity ",
        "fixed and understate the variance."))

      vis.refit.sums <- make_vis_refit_esc(
        rule         = visibility,
        donor.dat    = vis.res$donor.dat,
        boot.weights = boot.weights,
        esc.dat      = esc.dat,
        ec.dat       = ec.dat,
        cell.vars    = cell.vars,
        ego.id       = '.ego.id')

    } else {
      vis.refit <- make_vis_refit(
        rule         = visibility,
        donor.dat    = vis.res$donor.dat,
        boot.weights = boot.weights,
        ec.dat       = ec.dat,
        ego.id       = '.ego.id')
    }

    boot.ind.ests <- get_boot_ests_matrix(ec.dat, boot.weights, '.ego.id', cell.vars, 'ind',
                                          visibility = visibility,
                                          refit = vis.refit, refit_sums = vis.refit.sums)
    boot.agg.ests <- get_boot_ests_matrix(ec.dat, boot.weights, '.ego.id', cell.vars, 'agg')

    if (any(is.na(boot.ind.ests$asdr.hat))) {
      n.na <- sum(is.na(boot.ind.ests$asdr.hat))
      n.all <- length(boot.ind.ests$asdr.hat)
      warning(glue::glue("Individual estimates have {n.na} out of {n.all} values missing. These have been removed in the summary statistics. Beware!\n"))
    }

    # get estimated sampling uncertainty for the
    # individual and aggregate visibility estimates
    boot.ind.varest <- boot.ind.ests %>%
      ungroup() %>%
      group_by(across(all_of(cell.vars))) %>%
      summarise(asdr.hat.ci.low = quantile(asdr.hat, .025, na.rm=TRUE),
                asdr.hat.ci.high = quantile(asdr.hat, 0.975, na.rm=TRUE),
                asdr.hat.median = quantile(asdr.hat, 0.5, na.rm=TRUE),
                asdr.hat.se = sd(asdr.hat, na.rm=TRUE))

    if (any(is.na(boot.agg.ests$asdr.hat))) {
      n.na <- sum(is.na(boot.agg.ests$asdr.hat))
      n.all <- length(boot.agg.ests$asdr.hat)
      warning(glue::glue("Aggregate estimates have {n.na} out of {n.all} values missing. These have been removed in the summary statistics. Beware!\n"))
    }


    boot.agg.varest <- boot.agg.ests %>%
      ungroup() %>%
      group_by(across(all_of(cell.vars))) %>%
      summarise(asdr.hat.ci.low = quantile(asdr.hat, .025, na.rm=TRUE),
                asdr.hat.ci.high = quantile(asdr.hat, 0.975, na.rm=TRUE),
                asdr.hat.median = quantile(asdr.hat, 0.5, na.rm=TRUE),
                asdr.hat.se = sd(asdr.hat, na.rm=TRUE))

    # and join the estimated sampling uncertainty onto the returned asdrs
    asdr.ind.dat <- asdr.ind.dat %>%
      left_join(boot.ind.varest, by=cell.vars)

    asdr.agg.dat <- asdr.agg.dat %>%
      left_join(boot.agg.varest, by=cell.vars)

  }



  asdr.ind.dat <- asdr.ind.dat %>%
    rename(!!alter.sex := .sib.sex,
           alter.age = agelabel)

  asdr.agg.dat <- asdr.agg.dat %>%
    rename(!!alter.sex := .sib.sex,
           alter.age = agelabel)

  ec.dat <- ec.dat %>%
    rename(!!alter.sex := .sib.sex,
           !!ego.id := .ego.id,
           alter.age = agelabel,
           !!weights := .ego.weight)

  esc.dat <- esc.dat %>%
    rename(!!alter.sex := .sib.sex,
           !!ego.id := .ego.id,
           !!alter.id := .sib.id,
           alter.age = agelabel,
           !!frame.indicator := .sib.in.F,
           !!weights := .ego.weight)

  if(! is.null(cell.config$event.name)) {
    asdr.ind.dat$event.name <- cell.config$event.name
    asdr.agg.dat$event.name <- cell.config$event.name
    ec.dat$event.name <- cell.config$event.name
    esc.dat$event.name <- cell.config$event.name
  }

  res <- list(asdr.ind=asdr.ind.dat,
              asdr.agg=asdr.agg.dat,
              ec.dat=ec.dat,
              esc.dat=esc.dat)

  # if the user wants us to return all of the bootstrap estimates
  # (instead of just the summaries), add them to the results list
  if (! is.null(boot.weights)) {
    if(return.boot) {

      boot.ind.ests <- boot.ind.ests %>%
        rename(!!alter.sex := .sib.sex,
               alter.age = agelabel)

      boot.agg.ests <- boot.agg.ests %>%
        rename(!!alter.sex := .sib.sex,
               alter.age = agelabel)

      if(! is.null(cell.config$event.name)) {
        boot.ind.ests$event.name <- cell.config$event.name
        boot.agg.ests$event.name <- cell.config$event.name
      }

      res$boot.asdr.ind <- boot.ind.ests
      res$boot.asdr.agg <- boot.agg.ests

    }

  }

  ## Provenance travels with the estimate: which rule produced it, how many
  ## alters each tier resolved, and what share of the deaths and of the exposure
  ## were approximated. Attached rather than added as a column so that nothing
  ## downstream that indexes res by name is disturbed.
  attr(res, "vis_provenance") <- vis.res$provenance
  res$vis_provenance <- vis.res$provenance

  return(res)
}
