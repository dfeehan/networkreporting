#####################################################
## known_population.r
##
## the known population method for estimating respondents'
## degrees
##
## TODO -- handle NAs in code below (this is partially done)
## TODO -- make handling of column vars, etc, uniformly handled
##         when passed in as args. (ie, use get.var; now some fns
##         still don't use get.var)
## TODO -- when using defaults (for example, taking
##         popn size info from dataframe attributes,
##         should we print out a message to the screen?
##         or perhaps have a (default) verbose mode?)
## TODO -- think about code to get 45q15 from these data...

#####################################################
##' kp.degree.estimator
##'
##' compute an estimate of the respondents' degrees using
##' the known population method\cr
##'
##' note that this function does not take survey weights, since
##' these estimates are not for total degree, but just for the
##' individual degree of each respondent
##'
##' @param survey.data the dataframe with the survey results
##' @param known.popns if not NULL, a vector whose entries are the size of the known
##'                    populations, and whose names are the variable names in the dataset
##'                    corresponding to each one. if NULL, then assume that the survey.data
##'                    dataframe has an attribute called 'known.popns' containing this vector.
##' @param total.popn.size the size of the entire population. if NULL,
##'                        this function returns proportions; if not NULL, it
##'                        returns absolute numbers (ie, the proportions * total popn size)
##' @param na.rm if TRUE, disregard rows that have any missingness in
##'              the known populations; otherwise, use an adjusted estimator
##'              to produce those rows' degree estimates
##' @param verbose if TRUE, print messages to the screen
##' @return a vector with an estimate of the degree for each row
##'         in survey.data. if na.rm=TRUE, then the degree for rows that have
##'         missingness in the 'how many X' questions will be set
##'         to NA
##' @export
kp.degree.estimator <- function(survey.data,
                                known.popns=NULL,
                                total.popn.size=NULL,
                                na.rm=FALSE,
                                verbose=TRUE)
{

  if (is.null(known.popns)) {
    known.popns <- attr(survey.data, "known.popns")
  }

  total.popn.size <- parse.total.popn.size(total.popn.size,
                                           survey.data,
                                           verbose=verbose)

  if (! na.rm) {

    ## use the modified estimator: for each row, use the known
    ## population estimator, taking populations whose responses we
    ## don't have (ie are missing) out of the numerator and
    ## denominator
    
    kp.dat <- survey.data[, names(known.popns)]

    ## mask for missing values: this matrix has the same shape
    ## as kp.dat, but its entries are 1 if the entry in kp.dat is
    ## observed and 0 if missing
    ## NB: THIS appears to take a (relatively) long time to run...
    miss.mask <- aaply(kp.dat,
                       1,
                       function(row) {
                         return(as.numeric(! is.na(row)))
                       },
                       .expand=FALSE)

    ind.tot.known <- (rowSums(kp.dat, na.rm=TRUE))
    ## NB: THIS appears to take a (relatively) long time to run...    
    ind.overall.known <- aaply(miss.mask,
                               1,
                               function(row) {
                                 return(sum(row*known.popns))
                               })

    res <- ind.tot.known/ind.overall.known
    
  } else {

    tot.known <- (rowSums(subset(survey.data,
                                 select=names(known.popns))))    
    
    overall.known <- sum(known.popns)

    res <- tot.known/overall.known

  }

  if (! is.na(total.popn.size)) {
    res <- res * total.popn.size
  }

  return(res)

}

