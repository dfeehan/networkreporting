##' Get ego X sibling X cell reports
##'
##' Produces a long-form dataset that has a row for each respondent-sibling X cell combination
##'
##' @param sib.dat The long-form sibling history dataset (likely produced by TODO)
##' @param sib.id  String with the name of the column of \code{sib.dat} that has the sibling ID
##' @param ego.id  String with the name of the column of \code{sib.dat} that has the ID of the survey respondent
##' @param cell.config An object containing the configuration of cells; see TODO for more information
##' @return A tbl with a row for each respondent-sibling X cell
##' @examples
##'   # TODO
##' @export
get_esc_reports <- function(sib.dat,
                            # the name of the id of the sib in the sibling histories
                            sib.id,
                            # the name of the id of the ego in the sibling histories
                            ego.id,
                            ###
                            # these are all passed into occ.exp; see that fn for more details
                            cell.config) {

  # create .ego.id
  sib.dat <- sib.dat %>%
    mutate(.ego.id = !!sym(ego.id),
           .sib.id = !!sym(sib.id),
           # this constant weight is useful because we want to add up
           # within-ego totals
           .const_weight = 1)

  esc.reports <- occ.exp(data=sib.dat,
                         covars=c(".ego.id", ".sib.id", cell.config$covars),
                         start.obs=cell.config$start.obs,
                         end.obs=cell.config$end.obs,
                         event=cell.config$event,
                         age.groups=cell.config$age.groups,
                         age.offsets=cell.config$age.offset,
                         time.periods=cell.config$time.periods,
                         time.offsets=cell.config$time.offset,
                         weights='.const_weight',
                         exp.scale=cell.config$exp.scale)

  esc.reports <- esc.reports %>%
    rename(!!ego.id := .ego.id,
           !!sib.id := .sib.id,
           sib.occ = occ,
           sib.exp = exp)

  return(esc.reports)
}


