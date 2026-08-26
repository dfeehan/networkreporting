############################################################
##' Specify the cells (age groups, time period) to produce estimates for
##'
##' @param age.groups see Details
##' @param time.periods see Details
##' @param start.obs the column name with the time observation started
##' @param end.obs the column name with the time observation ended
##' @param event the column name with the time of the event (e.g. date of death)
##' @param age.offset the column name with each sibling's offset for age (typically the date of birth)
##' @param time.offset the column name with each sibling's offset for time (often the date of the survey interview)
##' @param covars (optional) vector with names of columns that have covariates
##' @param event.name (optional) name of the event (useful for maternal mortality)
##' @param exp.scale defaults to 1/12; see Details
##' @return A \code{cell_config} object that can be passed into estimation functions to describe
##' the cells that estimates should be produced for.
##'
##' @section Details:
##' * Note that all of the parameters that require column names are expecting strings. These
##'   column names refer to the dataset with one row for each reported sibling. This
##'   isn't passed into this function, so this function can't check that these column names are valid.
##' * \code{age.groups} can either be the output of one of the helper functions for creating age groups
##'   (\code{make.age.groups} or \code{make.even.age.groups}), or it can be '1yr', '5yr', or '10yr' for standard
##'   1, 5, or 10-year age groups ranging from 15 to 65, or '1yr_to50', '5yr_to50' for standard 1 or 5-year age
##'   groups ranging from 15 to 49
##' * \code{time.periods} can either be the output of \code{make.time.periods}, or it can be
##'   '12mo_beforeinterview', '5yr_beforeinterview', or '7yr_beforeinterview' for time periods one,
##'   five, or seven years before the interview date
##' * \code{exp.scale} is a factor that is used to convert differences between dates into years;
##'   this is usually 1/12, since the DHS reports times by month (eg Dec 2010 or Sept 1995). Differences
##'   in dates are thus denominated in months, and need to be multiplied by 1/12 to convert them into years.
##'
##' @export
cell_config <- function(age.groups,
                        time.periods,
                        start.obs,
                        end.obs,
                        event,
                        age.offset,
                        time.offset,
                        covars=NULL,
                        event.name = NULL,
                        exp.scale=1/12) {

  cell_config_res <- list()

  if(is.character(age.groups)) {
    if (age.groups == '1yr') {
      min.age <- 15
      max.age <- 65
      age.groups <- make.even.age.groups(1, min.age=min.age, max.age=max.age)
    } else if (age.groups == '1yr_to50') {
      min.age <- 15
      max.age <- 50
      age.groups <- make.even.age.groups(1, min.age=min.age, max.age=max.age)
    } else if (age.groups == '5yr') {
      min.age <- 15
      max.age <- 65
      age.groups <- make.even.age.groups(5, min.age=min.age, max.age=max.age)
    } else if (age.groups == '5yr_to50') {
      min.age <- 15
      max.age <- 50
      age.groups <- make.even.age.groups(5, min.age=min.age, max.age=max.age)
    } else if (age.groups == '10yr') {
      min.age <- 15
      max.age <- 65
      age.groups <- make.even.age.groups(10, min.age=min.age, max.age=max.age)
    } else {
      stop(glue::glue("No setting found for age.groups {age.groups}."))
    }
  } else {
    #stop("No age groups specified.")
    # using custom age group (which is OK)
  }

  if(is.character(time.periods)) {
    if(time.periods == '5yr_beforeinterview') {
      time.periods <- make.time.periods(start=-12*5,
                                        durations=12*5,
                                        names=c("5yr_beforeint"))
    } else if (time.periods == '7yr_beforeinterview') {
      time.periods <- make.time.periods(start=-12*7,
                                        durations=12*7,
                                        names=c("7yr_beforeint"))
    } else if (time.periods == '12mo_beforeinterview') {
      time.periods <- make.time.periods(start=-12,
                                        durations=12,
                                        names=c("12mo_beforeint"))
    } else {
      stop(glue::glue("No setting found for time.periods {time.periods}."))
    }
  } else if (is.null(time.periods)) {
    stop("No time periods specified.")
  } else {
    # using a custom time.periods object from make.time.periods() (which is OK,
    # and is what the documentation promises) -- mirrors the age.groups handling
    # above
  }

  cell_config_res$start.obs <- start.obs
  cell_config_res$end.obs <- end.obs
  cell_config_res$event <- event
  cell_config_res$age.groups <- age.groups
  cell_config_res$age.offset <- age.offset
  cell_config_res$time.periods <- time.periods
  cell_config_res$time.offset <- time.offset
  cell_config_res$event.name <- event.name
  cell_config_res$covars <- covars
  cell_config_res$exp.scale <- exp.scale

  class(cell_config_res) <- 'cell_config'

  return(cell_config_res)

}

############################################################
##' make labels for age groups
##'
##' helper function; see make.even.age.groups for more
##'
##' @param min.age the first age
##' @param max.age the highest age
##' @param aw the width of each group
##' @return see make.age.groups
agenames <- function(aw, min.age, max.age) {

  vals.lhs <- seq(from=min.age, to=(max.age-aw), by=aw)
  vals.rhs <- seq(from=(min.age+aw), to=max.age, by=aw)
  return(as.character(glue::glue('[{vals.lhs},{vals.rhs})')))
}

############################################################
##' make an age.groups object with evenly-sized intervals
##'
##' see make.age.groups for more
##'
##' @param min.age the first age
##' @param max.age the highest age
##' @param width the width of each group
##' @return see make.age.groups
##' @export
make.even.age.groups <- function(min.age, max.age, width) {
  widths <- rep(width*12, ((max.age-min.age)/width))
  names <- agenames(width, min.age, max.age)
  return(make.age.groups(start=min.age*12,
                         widths=widths,
                         names=names))
}

############################################################
##' make an age.groups object
##'
##' TODO -- would be nice if this could take either widths
##' or bins / breaks
##'
##' @param start the first age
##' @param widths the widths of the subsequent age groups
##' @param names the names of the age groups
##' @return an object (list) with the widths, names, and
##' number of age groups, as well as a matrix called
##' template which has the start and end of each age interval.
##' the intervals in template are close on the left but not
##' the right; eg, exp.start of 10 and exp.end of 20
##' means [10, 20) in terms of exact ages
##' (this is useful later on, for making individual age
##'  schedules based on, eg, birth dates)
##' @export
make.age.groups <- function(start, widths, names) {

  lhs <- start + c(0, cumsum(widths[-length(widths)]))

  rhs <- lhs + widths

  template <- cbind(exp.start=lhs, exp.end=rhs)

  if (is.null(names)) {
    names <- paste(lhs)
  } else if (length(names) > nrow(template)) {
    warning("too many names specified for age groups; truncating...")
    names <- names[1:nrow(template)]
  }

  return(list(widths=widths,
              names=names,
              template=template,
              num.groups=nrow(template)))
}

############################################################
##' make a time.periods object
##'
##' @param start the start of the time of interest
##' @param durations the durations of the subsequent time periods
##' @param names the names of the time periods
##' @return an object (list) with the widths, names, and
##' number of time periods
##' as well as a matrix called
##' template which has the start and end of each time period.
##' the intervals in template are closed on the left but not
##' on the right; that is, start of 1900 and end of 1910
##' means [1900, 1910) in terms of exact times.
##' @export
make.time.periods <- function(start, durations, names) {

  lhs <- start + c(0, cumsum(durations[-length(durations)]))

  rhs <- lhs + durations

  template <- cbind(start=lhs, end=rhs)

  if (is.null(names)) {
    names <- paste(lhs)
  } else if (length(names) > nrow(template)) {
    warning("too many names specified for time periods; truncating...")
    names <- names[1:nrow(template)]
  }

  return(list(durations=durations,
              names=names,
              template=template,
              num.groups=nrow(template)))
}
