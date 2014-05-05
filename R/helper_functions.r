#######################################################################
## helper_functions.R
##
## various helper functions for use in the networkreporting
## package
##

##########################################################################
##' get a variable from a dataframe or vector
##'
##' this function was written because a few of the estimator functions
##' need to use weights, and there are several cases to handle:
##' the user could pass in a column name, a vector of weights, or
##' nothing (in which case, the weights should default to 1 for each
##' row in the dataset). for the special case of getting weights, look
##' at the curried fn get.weights (right below)
##' 
##' @param survey.data the survey dataset
##' @param var either NULL, a column name, or a vector of values
##' @param default the default value to fill in if the variable
##'        is not found
##' @return a vector of values whose length is the same as the
##'         number of rows in survey.data; if var is NULL, this has
##'         the default values
##' @keywords internal
get.var <- function(survey.data, var, default=NA) {

  ## weights will default to 1 for everyone, unless the user specified
  ## a weights variable
  if (is.null(var)) {
    
    return(rep(default, nrow(survey.data)))
    
  } else if (length(var) == 1) {
  
    ## ... otherwise, see if the weights variable is referring
    ## to a column of the dataframe; try to
    ## grab sampling weights from survey dataframe
    var.vals <- try(subset(survey.data,
                           select=var),
                    silent=TRUE)

    if( inherits(var.vals, "try-error") ||
       ncol(var.vals) != 1 ||
       ! is.numeric(var.vals[,1]) ) {

      stop(paste(var,
                 " does not identify a valid column in the data.\n"))
    }

    var <- as.numeric(var.vals[,1])

    return(var)
    
  } else if (is.numeric(var) & length(var) == nrow(survey.data)) {

    ## if var a vector with one entry per row, then these
    ## are our values
    return(var)
  } else {    
    stop("can't determine what the values should be for ", var, ".")
  }
    
  
}

##########################################################################
##' get the weights column from a dataframe
##'
##' this is the same as get.var with the default value set to 1
##' instead of NA
##' @param ... (this is a function curried from \code{get.var})
##' @keywords internal
get.weights <- functional::Curry(get.var, default=1)

##########################################################################
##' grab a function based on its name
##'
##' helper to grab a fn that is passed in as an argument
##' 
##' this is based on Hadley Wickham's response to an SO
##' post: \url{http://stackoverflow.com/questions/14183766/match-fun-provide-error-with-functions-defined-inside-functions}
##' with some minor modifications
##'
##' @param fn the function to search for
##' @param env the environment to start searching in
##' @return fn, if fn is already a function; otherwise, the first function found
##'         in env or one of its parents whose name is fn
##' @keywords internal 
get.fn <- function(fn, env = parent.frame()) {

    ## base case: fn is already a function
    if (is.function(fn)) {
      return(fn)
    }
  
    ## base case: nothing left to search through
    if (identical(env, emptyenv())) {
        stop("Could not find function ", fn, "!")
    }

    ## base case: found function in env
    if (exists(fn, env, inherits=FALSE) &&
        is.function(env[[fn]])) {
        return(env[[fn]])

    ## recursive case: look through the environment
    ## above env
    } else {
        return(get.fn(fn, parent.env(env)))
    }

}

##########################################################################
##' turn a dataframe into a known population vector
##'
##' \code{df.to.kpvec} takes a dataframe which has a column with
##' known population names, and a column with known population
##' totals, and turns it into a known population vector. if the
##' names of the survey variables corresponding to each known population
##' are available, they can be passed in as well
##'
##' @param kp.data the known population dataset
##' @param kp.var the column of \code{kp.data} that has known population names;
##'               either a column name, a column index, or a vector of values
##' @param kp.value the column of \code{kp.data} that has known population sizes;
##'               either a column name, a column index, or a vector of value
##' @return a vector whose entries have the known population values and whose
##' names have the corresponding \code{kp.var} value
##' @export
##' @seealso \link{add.kp}
##' @examples \dontrun{
##'   # if kp.dat is a dataframe with columns 'kp' with known popn names
##'   # and 'total.size' with the total size,
##'   # and my.survey is the dataframe with survey responses
##'
##'   my.kp.vec <- df.to.kpvec(kp.data, kp.var='kp', kp.value='total.size')
##'   my.survey <- add.kp(my.survey, my.kp.vec)
##'
##'   # now we can call estimator functions like
##'   # kp.degree.estimator without having to specify known
##'   # populations each time
##' }
##'
df.to.kpvec <- function(kp.data,
                        kp.var,
                        kp.value) {

  vals <- get.var(kp.data, kp.value)
  var <- get.var(kp.data, kp.var)

  kp.vec <- vals
  names(kp.vec) <- var

  return(kp.vec)

}

##########################################################################
##' attach known populations to a dataframe
##'
##' @description
##' take a known population vector (see \code{\link{df.to.kpvec}}) and
##' associate it with a survey dataframe. this makes it more convenient
##' to use some of the \code{networkreporting} package's function
##'
##' @details
##' The \code{total.popn.size} parameter is interpreted as follows:
##' \itemize{
##' \item NA if total.popn.size is NA then work with proportions
##' \item NULL if total.popn.size is NULL (nothing passed in), then
##'          assume that there's a total.popn.size attribute
##'          associated with the dataset we're using
##' \item numerical value if an actual total.popn.size was passed in,
##'        use that value
##' }
##' 
##'
##' @param survey.data the survey dataframe
##' @param kp.vec the known population vector
##' @param total.pop.size (optional) the total population size to use (see below)
##' @return the survey dataframe with the known population vector
##' attached as an attribute
##' @export
##' @seealso \link{df.to.kpvec}
##' @examples \dontrun{
##'
##'   # if kp.dat is a dataframe with columns 'kp' with known popn names
##'   # and 'total.size' with the total size,
##'   # and my.survey is the dataframe with survey responses
##'
##'   my.kp.vec <- df.to.kpvec(kp.data, kp.var='kp', kp.value='total.size')
##'   my.survey <- add.kp(my.survey, my.kp.vec)
##'
##'   # now we can call estimator functions like
##'   # kp.degree.estimator without having to specify known
##'   # populations each time
##' }
add.kp <- function(survey.data, kp.vec, total.pop.size=NULL) {
  attr(survey.data, "known.popns") <- kp.vec

  if (! is.null(total.pop.size)) {
      attr(survey.data, "total.popn.size") <- total.pop.size
  }
    
  return(survey.data)
}

##########################################################################
##' get a variable from a dataframe or vector
##'
##' this function was written because a few of the estimator functions
##' need to use weights, and there are several cases to handle:
##' the user could pass in a column name, a vector of weights, or
##' nothing (in which case, the weights should default to 1 for each
##' row in the dataset). for the special case of getting weights, look
##' at the curried fn get.weights (right below)
##'
##' @param survey.data the survey dataset
##' @param var either NULL, a column name, or a vector of values
##' @param default the default value to fill in if the variable
##'        is not found
##' @return a vector of values whose length is the same as the
##'         number of rows in survey.data; if var is NULL, this has
##'         the default values
##' @keywords internal
get.var <- function(survey.data, var, default=NA) {

  ## weights will default to 1 for everyone, unless the user specified
  ## a weights variable
  if (is.null(var)) {

    return(rep(default, nrow(survey.data)))

  } else if (length(var) == 1) {

    ## ... otherwise, see if the weights variable is referring
    ## to a column of the dataframe; try to
    ## grab sampling weights from survey dataframe
    var.vals <- try(survey.data[,var,drop=FALSE],
                    silent=TRUE)

    ## if( inherits(var.vals, "try-error") ||
    ##    ncol(var.vals) != 1 ||
    ##    ! is.numeric(var.vals[,1]) ) {
    if( inherits(var.vals, "try-error") ||
       ncol(var.vals) != 1) {

      stop(paste(var,
                 " does not identify a valid column in the data.\n"))
    }

    var <- var.vals[,1]

    return(var)

  } else if (length(var) == nrow(survey.data)) {

    ## if var a vector with one entry per row, then these
    ## are our values
    return(var)
  } else {
    stop("can't determine what the values should be for ", var, ".")
  }


}

##########################################################################
##' get the weights column from a dataframe
##'
##' this is the same as get.var with the default value set to 1
##' instead of NA
##' @param ... (this is a function curried from \code{get.var})
##' @keywords internal
get.weights <- functional::Curry(get.var, default=1)

##########################################################################
##' only prints things out in verbose mode
##'
##' @param verbose if TRUE, print things out; otherwise, do nothing
##' @param ... arguments to pass to cat if verbose is TRUE
##' @keywords internal
vcat <- function(verbose=TRUE, ...) {

  if(verbose) {
    message(...)
  }

  invisible()
}

##########################################################################
##' parse a formula that describes the design of a survey
##'
##' Given a formula of the form\cr
##' \code{~ psu_v1 + psu_v2 + ... + strata(strata_v1 + strata_v2 + ...)}\cr
##' \itemize{
##'  \item{"psu.formula"}{a formula describing the primary sampling unit vars}
##'  \item{"strata.formula"}{a formula describing the strata (if any)}
##' }\cr
##'
##' @param formula a formula describing the sample design (see above)
##' @return a list with entries \code{psu.formula} and \code{strata.formula}
##' @keywords internal
parse_design <- function(formula) {

  # TODO -- potential future improvements
  # check to be sure no response is included (or warn)
  # check formulas for strata more carefully

  ## see http://stackoverflow.com/questions/10224805/how-to-select-a-part-of-formula-in-formula-in-r
  ## for some helpful info

  psu.formula <- formula
  strata.formula <- NULL

  these.labels <- attr(terms(formula), "term.labels")

  strata.idx <- grep("strata\\(", these.labels)

  if (length(strata.idx) == 1) {

    # grab the expression in the strata(...) part of the formula
    #strata.text <- str_match

    strata.text <- stringr::str_match(these.labels[strata.idx],
                                      "strata\\((.+)\\)")[2]

    ## updating instead of creating a new formula b/c this preserves
    ## the environment that the original formula was created in...
    strata.formula <- update(formula,
                             paste("~ ", strata.text))

    psu.formula <- update.formula(formula,
                                  paste("~ . - strata(",strata.text,")"))

  } else if (length(strata.idx > 1)) {

    stop("Cannot have more than one strata() specification in the design formula.")
  }

  return(list(psu.formula=psu.formula,
              strata.formula=strata.formula))

}


##########################################################################
##' topcode a vector of numerical values
##'
##' this function topcodes one vector; it's used by the \code{topcode}
##' function to topcode a set of columns in a data frame
##'
##' @param x the vector of values to topcode
##' @param max the maximum value; all values > max are recoded to max
##' @param to.na a vector of values to recode to NA (this happens before topcoding)
##' @param ignore a vector of values to leave unchanged
##' @return the topcoded vector
##' @export
topcode.var <- function(x, max, to.na=NULL, ignore=NA) {

  # TODO -- write example

  if (! is.numeric(x)) {
    stop("You can only topcode a numeric vector.")
  }

  if (! is.null(to.na)) {
    x[x %in% to.na] <- NA
  }

  x[(x > max) & (! x %in% ignore)] <- max

  return(x)
}

##########################################################################
##' topcode a group of variables
##'
##' this function uses \code{topcode.var} to topcode a set of variables.
##' it's useful for topcoding a whole set of aggregated relational data
##' ("how many X are you connected to?") questions in the same way.
##'
##' @param survey.data  the dataset with the survey responses
##' @param vars a vector with the names or indices of the columns in the
##'             dataframe that are to be topcoded
##' @param max the maximum value; all values > max are recoded to max
##' @param ignore a vector of values to leave unchanged
##' @param to.na a vector of values to recode to NA (this happens before topcoding)
##' @return the topcoded vector
##' @export
##' @examples \dontrun{
##'    data(hh.survey) # example data included with the package
##'    example.survey <- topcode.data(example.survey,
##'                                   vars=known.popn.vars,
##'                                   max=30)
##' }
topcode.data <- function(survey.data, vars, max, to.na=NULL, ignore=NA) {

  ## TODO -- eventually check that vars are found in the columns of survey.data

  survey.data[,vars] <- colwise(topcode.var)(survey.data[,vars,drop=FALSE],
                                             max=max,
                                             to.na=to.na,
                                             ignore=ignore)

  return(survey.data)

}


##########################################################################
##' compute the weighted mean
##'
##' given a vector of values and a vector of weights, compute the
##' weighted mean
##'
##' @param x the vector of values
##' @param w the vector of weights
##' @param na.rm if TRUE, only consider elmeents of x that are not missing
##'              (and their corresponding entries in w). Defaults to FALSE.
##' @return the weighted mean
##' @keywords internal
weighted.mean <- function(x, w, na.rm=FALSE) {

  if (na.rm) {
    idx <- (1:length(x))[!is.na(x)]
  } else {
    idx <- 1:length(x)
  }

  return(sum(x[idx]*w[idx])/sum(w[idx]))
}



##########################################################################
##' handle the total.popn.size argument in a uniform way across
##' several functions
##'
##' handle the total.popn.size argument in a uniform way across
##' several functions, including
##' \code{\link{kp.degree.estimator}},
##' \code{\link{nsum.internal.validation}}, and
##' \code{\link{nsum.estimator}}.
##'
##' The result depends upon the value that was passed in:
##' \itemize{
##' \item NA if total.popn.size is NA then work with proportions
##' \item NULL if total.popn.size is NULL (nothing passed in), then
##'          assume that there's a total.popn.size attribute
##'          associated with the dataset we're using
##' \item numerical value if an actual total.popn.size was passed in,
##        use that value
##' }
##'
##' @param total.popn.size value to parse
##' @param survey.data the dataframe we're analyzing, which may or may not
##'                    have an attribute called 'total.popn.size'
##' @param verbose if TRUE, print messages to the screen
##' @return the parsed total population size
##' @keywords internal
parse.total.popn.size <- function(total.popn.size, survey.data, verbose=FALSE) {

  ## this is a little complicated.
  ## (see also kp.degree.estimator)
  if ((! is.null(total.popn.size)) && is.na(total.popn.size)) {

    vcat(verbose, "working in proportions\n")

  } else if (is.null(total.popn.size)) {

    total.popn.size <- attr(survey.data, "total.popn.size")

    if(! is.numeric(total.popn.size)) {
      stop("error - no suitable attribute 'total.popn.size' for dataframe.\n")
    } else {
      vcat(verbose, "using dataframe's attribute for total population size.\n")
    }

  } else {
    vcat(verbose, "working in absolute numbers\n")
  }

  return(total.popn.size)
}

##########################################################################
##' given an estimated subpopn size or prevalence and the correct value,
##' produce some measurements of how close the esimate is
##'
##' @param estimate the estimate
##' @param truth the correct answer
##' @return a vector whose entries have various summaries of fit
##' @export
estimate.error <- function(estimate, truth) {

  # TODO -- write example

  err <- estimate - truth
  abserr <- abs(err)
  sqerr <- err^2
  relerr <- abserr/truth

  return(cbind(err=err,abserr=abserr,sqerr=sqerr,relerr=relerr))

}
