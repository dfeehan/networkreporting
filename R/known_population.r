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
##' @param survey.data the dataframe with the survey results
##' @param known.popns if not NULL, a vector whose entries are the size of the known
##'                    populations, and whose names are the variable names in the dataset
##'                    corresponding to each one. if NULL, then assume that the survey.data
##'                    dataframe has an attribute called 'known.popns' containing this vector.
##' @param total.popn.size the size of the entire population. if NULL,
##'                        this function returns proportions; if not NULL, it
##'                        returns absolute numbers (ie, the proportions * total popn size)
##' @param weights if not NULL, weights to use in computing the estimate. this
##'                should be the name of the column in the survey.data which has
##'                the variable with the appropriate weights. these weights
##'                should be construted so that, eg, the mean of the degrees is
##'                estimated as (1/n) * \\sum_i {w_i * d_i}
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
                                weights=NULL,
                                na.rm=FALSE,
                                verbose=TRUE)
{

  if (is.null(known.popns)) {
    known.popns <- attr(survey.data, "known.popns")
  }

  total.popn.size <- parse.total.popn.size(total.popn.size,
                                           survey.data,
                                           verbose=verbose)

  ## weights will default to 1 for everyone, unless the user specified
  ## a weights variable
  weights <- get.weights(survey.data, weights)
  
  if (! na.rm) {

    ## use the modified estimator: for each row, use the known
    ## population estimator, taking populations whose responses we
    ## don't have (ie are missing) out of the numerator and
    ## denominator
    
    kp.dat <- survey.data[, names(known.popns)]

    ## mask for missing values: this matrix has the same shape
    ## as kp.dat, but its entries are 1 if the entry in kp.dat is
    ## observed and 0 if missing
    miss.mask <- aaply(kp.dat,
                       1,
                       function(row) {
                         return(as.numeric(! is.na(row)))
                       },
                       .expand=FALSE)

    ind.tot.known <- (rowSums(kp.dat, na.rm=TRUE) * weights)
    ind.overall.known <- aaply(miss.mask,
                               1,
                               function(row) {
                                 return(sum(row*known.popns))
                               })

    res <- ind.tot.known/ind.overall.known
    
  } else {

    tot.known <- (rowSums(subset(survey.data,
                                 select=names(known.popns))) * weights)
    
    overall.known <- sum(known.popns)

    res <- tot.known/overall.known

  }

  if (! is.na(total.popn.size)) {
    res <- res * total.popn.size
  }

  return(res)

}

