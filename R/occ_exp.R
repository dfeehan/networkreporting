##########################################################
##' tabulate occurrences and exposures
##'
##' Given a variable indicating when an event happened,
##' a time window we are interested in, and possibly a set
##' of covariates, tabulate counts of event occurences and
##' exposures in the given time interval.\cr
##' Note that you have to be careful about observations that
##' don't experience an event, but still count for exposure;
##' see the example below.
##'
##'
##' TODO
##' \itemize{
##'    \item write unit tests
##'    \item fill an example in the documentation below
##'    \item at start of code, handle defaults more elegantly
##'    \item id.var is not implemented; might be better to directly handle
##'          multiple events (see below)
##'    \item handle multiple time periods
##'    \item handle multiple events
##'    \item handle GK weights (possibly not in this function)
##'    \item possibly refactor in the future to pass in dataset of lifelines
##'          and separate list of event dates
##'    \item what if event date is missing?
##'    \item what about things that vary with the event, eg mother's
##'                  age when child was born?
##'    \item better description of dates; this was developed using CMC codes
##'          from the DHS surveys, but it should work for any interval scale
##' }
##'
##' @examples
##' \dontrun{
##'   ## THESE EXAMPLES ARE NOT UPDATED!
##'   ## Please disregard for the time being...
##'
##'   ## RECODE so that observations w/ no births show up
##'   ## in the dataset at least once by giving them a first
##'   ## birth at the (impossible) CMC code of -1. This ensures
##'   ## that they never contribute a birth, but that they
##'   ## still count for exposure.
##'   ##
##'   ## NB: this is a key step. if we don't do this,
##'   ## women who haven't had any births are removed
##'   ## from the dataset, biasing rates upward...
##'
##'   bdata.coded <- bdata
##'   bdata.coded$bdate[ is.na(bdata.coded$bdate) &
##'                     bdata.coded$bnum == "01" ] <- -1
##'   bdata.coded <- subset(bdata.coded, ! is.na(bdate) )
##'
##'   ## NO COVARIATES:
##'   ## now use compute.occ.exp to get counts of
##'   ## births and exposure between 1980 and 1990
##'   ## for ages 0 to 60
##'
##'   ## TODO -- need to write this example using new
##'   ##         version
##'
##'   ## WITH COVARIATES:
##'   ##  use compute.occ.exp to get counts of
##'   ## births and exposure by 5-year period
##'   ## between 1970 and 2005,
##'   ## for 5-year age groups [0,5), ..., [60,65)
##'   ## by the covariates
##'   ## urban, highestedlevel, and religion
##'   ## (NOTE: this is just illustrative. we wouldn't recommend
##'   ##  substantively interpreting the results of this example.)
##'
##'   ## TODO -- need to write this example using new
##'   ##         version
##' }
##'
##' @param data the dataset containing events
##' @param start.obs vector of values (one per row of \code{data}) with the starting point
##'                  of the observation window for each row, in CMC format
##' @param end.obs   vector of values (one per row of \code{data}) with the ending point
##'                  of the observation window for each row, in CMC format
##' @param event the column of the dataset that indicates the date of an event. observations
##'        that contribute exposure but no events should have this set to a value that
##'        will never occur in the time period; for example, -1
##' @param age.groups an age.groups object
##' @param age.offsets if not NULL, then the age.periods are to be interpreted relative to
##'        these times (one for each row). this is usually a birth date
##' @param time.periods a time.periods object
##' @param time.offsets if not NULL, then the time.periods are to be interpreted relative to
##'        these times (one for each row). useful for computing quantities like
##'        "X months before interview", where interview happened at different times
##'        for different respondents
##' @param id.var the variable giving the unique rows of the dataset for each
##'        individual (UNDER DEVELOPMENT)
##' @param covars the name of covariates over which occurrences and exposures should be
##'        aggregated; defaults to NULL, meaning totals are computed over the entire dataset
##' @param weights the weight to apply to occurrences and exposures; defaults to 1
##' @param discretize if TRUE, turn reported amounts of exposure into a 0/1 exposure or no exposure;
##'        0 exposure when < half of the time interval was spent in the cell and 1 exposure when >= half
##'        of the time interval was spent in the cell; thus, each sib can contribute to exposure in one and only one cell
##' @param exp.scale amount by which to scale exposure; if, for example, dates are measured
##'                   in months, but you want to measure rates in years, then this should
##'                   be 1/12. It defaults to 1
##' @return a data frame with the covariates, age groups, occurences and expsoures
##'
##' @export
occ.exp <- function(data,
                    start.obs,
                    end.obs,
                    event,
                    age.groups,
                    time.periods,
                    id.var=NULL,
                    covars=NULL,
                    age.offsets=NULL,
                    time.offsets=NULL,
                    weights=NULL,
                    discretize=FALSE,
                    exp.scale=1)
{


  ## TODO - I think the next several blocks (handling defaults)
  ## could be improved

  data <- data %>% mutate(.start.obs = !!sym(start.obs),
                          .end.obs = !!sym(end.obs),
                          .event = !!sym(event))

  if (is.null(id.var)) {
    data$.id <- 1:nrow(data)
  } else {
    #data$.id <- data[,id.var]
    data <- data %>% mutate(.id = !!sym(id.var))
  }
  id.var <- ".id"

  if (is.null(age.offsets)) {
    data$.age.offset <- 0
  } else {
    #data$.age.offset <- data[,age.offsets]
    data <- data %>% mutate(.age.offset = !!sym(age.offsets))
  }
  age.offsets <- ".age.offset"

  if (is.null(time.offsets)) {
    data$.time.offset <- 0
  } else {
    #data$.time.offset <- data[,time.offsets]
    data <- data %>% mutate(.time.offset = !!sym(time.offsets))
  }
  time.offsets <- ".time.offset"

  if (is.null(weights)) {
    data$.weight <- 1
  } else {
    #data$.weight <- data[,weights]
    data <- data %>% mutate(.weight = !!sym(weights))
  }
  weights <- ".weight"

  if (discretize) {
    if(exp.scale != 1) {
      warning("Warning: you are discretizing but exp.scale is not equal to 1. This is probably a mistake.")
    }
  }

  ## we'll need this to use the programmatic version of the dply
  ## aggregation functions below
  ## see
  ## http://stackoverflow.com/questions/21208801/group-by-multiple-columns-in-dplyr-using-string-vector-input
  #gpvars <- lapply(covars, as.symbol)
  gpvars <- covars

  #full.dat <- data %>% select_(.dots=c(start.obs, end.obs, event, id.var,
  #                                     age.offsets, time.offsets,
  #                                     weights, covars))
  full.dat <- data %>% select(.start.obs, .end.obs, .event,
                              .id,
                              .age.offset,
                              .time.offset,
                              .weight,
                              !!!covars)

  ## NOTE: if future versions allow multiple events, there
  ##  will be a separate list that maps ids to event lists
  ##  instead of having the third column of this matrix
  ##  have the event date (if any)
  lifeline.mat <- as.matrix(full.dat %>% select(1:3))
  colnames(lifeline.mat) <- c("start.obs", "end.obs", "event")

  ## helper fn used below
  weighted.sum <- function(x, w) {
    return(sum(x*w))
  }

  uber.res <- purrr::map_dfr(1:time.periods$num.groups,
                    function(time.idx) {

                      this.time.period <- matrix(time.periods$template[time.idx,],
                                                 nrow=nrow(lifeline.mat),
                                                 ncol=2,
                                                 byrow=TRUE)


                      toff <- full.dat %>% select(.time.offset) %>% pull(1)

                      this.time.period <- this.time.period + toff

                      this.age.groups <- age.groups$template

                      this.age.offset <- full.dat %>% select(.start.obs) %>% pull(1)


                      if(discretize) {
                        cat("Discretizing...")
                        raw.res <- cpp_compute_occ_exp2(lifeline.mat,
                                                        this.age.groups,
                                                        this.age.offset,
                                                        this.time.period,
                                                        # SUPER TEMP - hard code threshold
                                                        thresh=30)
                      } else {
                        raw.res <- cpp_compute_occ_exp(lifeline.mat,
                                                       this.age.groups,
                                                       this.age.offset,
                                                       this.time.period)
                      }


                      ## summarize each qty (occ and exp) separately; then combine them
                      agg.qty <- purrr::map_dfr(c('occ', 'exp'),
                                       function(this.qty) {

                                         res.qty <- as.data.frame(raw.res[[this.qty]])
                                         colnames(res.qty) <- paste0("agegroup_", 1:ncol(res.qty))

                                         # note that this assumes the order of the rows hasn't changed
                                         res.qty <- cbind(full.dat, res.qty)

                                         res.qty.agg <- res.qty %>%
                                           group_by(across(all_of(gpvars))) %>%
                                           summarise(across(starts_with("agegroup"),
                                                            ~ weighted.sum(.x, .weight)),
                                                     .groups = "drop")

                                         res.qty.agg <- res.qty.agg %>%
                                           tidyr::pivot_longer(cols = tidyselect::starts_with("agegroup"),
                                                               names_to = 'agegroup',
                                                               values_to = 'value') %>%
                                           mutate(qty=this.qty)

                                         return(res.qty.agg)

                                       })

                      agg.qty$time.period <- time.periods$names[[time.idx]]

                      return(agg.qty)

                    })

  agg.res <- tidyr::pivot_wider(uber.res, names_from = qty, values_from = value)

  ## rename the age group to match the definitions
  agename.remap <- data.frame(agegroup=paste0("agegroup_", 1:length(age.groups$names)),
                              agelabel=age.groups$names,
                              stringsAsFactors=FALSE)

  agg.res$agegroup <- paste(agg.res$agegroup)

  agg.res <- left_join(agg.res, agename.remap, by='agegroup')

  ## rescale the exposure (for example, if our time units are months,
  ## as with DHS CMC codes, we'd want to scale by 1/12 to be able to
  ## produce yearly rates)
  agg.res <- agg.res %>% mutate(exp=exp*exp.scale)

  ## TODO
  ##  -> test with fixed time periods (instead of time before interview)
  ##  -> allow multiple time periods (shouldn't be too hard)
  ##  -> allow multiple events (eg for fertility)
  ##  -> develop unit tests
  ##  -> be sure to test with missing data

  return(agg.res)

}

