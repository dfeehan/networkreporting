##' get ego X cell reports
##'
##' Produces a dataframe that has a row for each respondent X cell containing respondent's reported deaths and exposure among siblings in the cell.
##'
##' @param esc.dat Dataset with a row for each respondent X sibling X cell, likely produced by \code{\link{get_esc_reports}}
##' @param ego.id  String with the name of the column in \code{esc.dat} containing the survey respondent's id
##' @param sib.dat Dataset with a row for each reported sibling, likely produced by TODO
##' @param sib.frame.indicator String with the name of the column in \code{sib.dat} containing a 0/1 coded variable indicating whether or not each sib is in the frame population
##' @param cell.vars a vector with Strings containing the names of columns in \code{esc.dat} identifying the cells to group reports by (typically age group, sex, time period)
##' @param weights String with the name of the column in \code{esc.dat} that has the survey weights
##' @param ind.vis.var String with the name of the column in \code{esc.dat} that has the individual visibility for each sibling; defaults to NULL. If NULL, the individual visibilities are calculated
##' @param discretize.exp Boolean for whether or not expsoure should be discretized. Not yet implemented.
##' @param check.frame.consistency Boolean; if TRUE (the default), warn when any
##'        occurrence is attributed to an alter marked as being in the frame
##'        population. For mortality that is impossible and indicates a miscoded
##'        frame indicator. Set FALSE when the event being counted is not death.
##' @return A dataframe that has a row for each respondent X cell containing respondent's reported deaths and exposure among siblings in the cell.
##' @examples
##'   # TODO - add example
##' @export
get_ec_reports <- function(esc.dat,
                           ego.id,
                           sib.dat,
                           sib.frame.indicator,
                           cell.vars,
                           # the survey weight for each respondent
                           weights,
                           # the name of the column w/ individual visibilities
                           ind.vis.var = NULL,
                           discretize.exp = FALSE,
                           check.frame.consistency = TRUE) {

  esc.dat <- esc.dat %>% dplyr::rename(.ego.id = !!sym(ego.id),
                                       .sib.in.F = !!sym(sib.frame.indicator))

  sib.dat <- sib.dat %>% dplyr::rename(.ego.weight = !!sym(weights),
                                       .ego.id = !!sym(ego.id))

  if(discretize.exp) {
    stop("discretize.exp not yet implemented...")
  }

  # if individual visibility weights have not been calculated,
  # do so...
  if(is.null(ind.vis.var)) {

    # add sibling individual visibility weights to the ESC dataset
    esc.dat.with.indviswgt <- add_esc_ind_vis(esc.dat,
                                              ego.id,
                                              sib.dat,
                                              sib.frame.indicator,
                                              varname='ind_vis')

  } else {

    esc.dat.with.indviswgt <- esc.dat %>%
      dplyr::rename(ind_vis = !!sym(ind.vis.var))

  }

  ## ego reports about cells
  ##
  ## TODO - eventually add yprime quantities?
  res <- esc.dat.with.indviswgt %>%
    ungroup() %>%
    group_by(across(all_of(c('.ego.id', cell.vars)))) %>%
    summarize(# this should be constant within respondents (.ego.id)
              y.F = y.F[1],
              # for aggregate vis
              y.Dcell = sum(sib.occ),
              y.Ncell = sum(sib.exp),
              y.NandFcell = sum(.sib.in.F * sib.exp),
              # occurrences split by frame status, the same way exposure is.
              # For mortality y.DandFcell is identically zero, since a dead
              # alter is never on the frame -- but that degeneracy is a fact
              # about mortality, not about the framework, and splitting every
              # reported quantity the same way is what makes the cheap
              # bootstrap identity uniform over quantities rather than
              # special-cased to occurrence and exposure.
              y.DandFcell = sum(.sib.in.F * sib.occ),
              # for individual vis (use individual vis weights)
              y.Dcell.ind = sum(sib.occ*ind_vis),
              y.Ncell.ind = sum(sib.exp*ind_vis),
              .groups = "drop"
              ) %>%
    mutate(y.NandnotFcell = y.Ncell - y.NandFcell,
           y.DandnotFcell = y.Dcell - y.DandFcell)

  ## A non-zero y.DandFcell means an alter is recorded as both dead and in the
  ## frame population, which is a frame-definition or coding error rather than
  ## a possible state of the world. Checked here because this is the first
  ## point at which the two facts meet, and because nothing else in the package
  ## looks for it. Verified zero across the Rwanda 2010 DHS and Zimbabwe 2019
  ## MICS prep paths.
  if (check.frame.consistency) {
    n.bad <- sum(res$y.DandFcell != 0, na.rm = TRUE)
    if (n.bad > 0) {
      warning(glue::glue(
        "{n.bad} ego X cell row(s) report an occurrence for an alter marked as ",
        "being IN the frame population (y.DandFcell != 0).\n",
        "For mortality that cannot happen: a dead alter is never on the frame, ",
        "so this points at the frame indicator being miscoded rather than at a ",
        "genuine feature of the data. Estimates will still be produced.\n",
        "Pass check.frame.consistency = FALSE to silence this if the event ",
        "being counted is not death."))
    }
  }

  ## res now has a row for each ego X cell
  ## we want to add sampling weight info to this dataset

  # join sampling weights onto sibship visibilities
  #yFdat <- yFdat %>% left_join(sib.dat %>%
  #                             group_by(.ego.id) %>%
  #                             slice(1) %>%
  #                             select(.ego.id, .ego.weight), by='.ego.id')

  ## TODO - LEFT OFF HERE
  ##   ... figuring this out; needed for IC checks...
  ## TODO - not yet calculating y.Fcell and y.Fnotcell
  ##        think this through once we get the other stuff working
  #res <- res %>%
  #  left_join(yFdat, by='.ego.id')
  #%>%
  #mutate(#y.Fnotcell = y.F - y.Fcell,
  #       y.NandnotFcell = y.Ncell - y.NandFcell)

  wgts <- sib.dat %>%
    group_by(.ego.id) %>%
    slice(1) %>%
    select(.ego.id, .ego.weight)

  res <- res %>%
    left_join(wgts, by='.ego.id') %>%
    rename(!!ego.id := .ego.id,
           !!weights := .ego.weight)

  return(res)
}
