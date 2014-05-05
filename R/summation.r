#####################################################
## summation.R
##
## the summation method for estimating respondents'
## degrees

#####################################################
##' summation.degree.estimator
##'
##' compute an estimate of the respondents' degrees using
##' the summation method (McCarty et al 2001)
##'
##' This is the method first described in McCarty et al 2001,
##' "Comparing two methods for estimating network size"
##'
##' Note that the summation degree estimator for the
##' case where there is missing data is not yet implemented.
##' (In fact, I don't think that there is a known
##' estimator for this case.)
##'
##' @param survey.data the dataframe with the survey results
##' @param sum.q if not NULL, a vector whose entries are the variable
##'              names in the dataset corresponding to each summation
##'              question. if NULL, then assume that
##'              the survey.data dataframe has an attribute called 'sum.qs'
##'              containing this vector.
##' @param missing if "ignore", then proceed with the analysis without
##'                doing anything about missing values. other options
##'                are not yet implemented.
##' @return a vector with an estimate of the degree for each row
##'         in survey.data. if na.rm=TRUE, then the degree for rows that have
##'         missingness in the summation questions will be set
##'         to NA
##' @export
summation.degree.estimator <- function(survey.data,
                                       sum.q=NULL,
                                       missing="ignore")
{

  # TODO -- turn text cite above into actual cite

  if (is.null(sum.q)) {
    sum.q <- attr(survey.data, "sum.q")
  }

  if (is.null(sum.q)) {
    stop("no sum.q provided for computing degrees via the summation method.\n")
  }
  if (missing != "ignore") {

    ## TODO -- FILL THIS IN
    stop("this is not implemented for the case where missing != 'ignore'.\n")

  } else {

    res <- rowSums(subset(survey.data,
                          select=sum.q))

  }

  return(res)

}
