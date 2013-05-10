\name{alter.distn.estimator}
\alias{alter.distn.estimator}
\title{estimate the degree distribution of the set of alters using
a direct approach.}
\usage{
  alter.distn.estimator(resp.data, attribute.names,
    d.hat.vals = "d.hat", dropmiss = FALSE, weights = NULL)
}
\arguments{
  \item{resp.data}{the dataframe that has the estimated
  degree of each respondent, along with the same
  characteristics reported for each alter that are found in
  attribute.data. note that the estimated degree should be
  in the column indicated by d.hat.col, and that the column
  names of the attributes should match their names in
  attribute.data}

  \item{attribute.names}{if not NULL, the names of the
  columns of the respondent data that contain the
  respondent's attributes. if NULL, then the names are
  taken to be the non-degree columns of resp.data}

  \item{d.hat.vals}{the name or index of the column with
  the degree estimates in resp.data; all the other columns
  are assumed to be attributes}

  \item{dropmiss}{if TRUE, drop combinations of attributes
  that have missing average degree estimates; if TRUE, keep
  them}

  \item{weights}{if not NULL, weights to use in computing
  the estimate. this should be the name of the column in
  the resp.data which has the variable with the appropriate
  weights. these weights should be construted so that, eg,
  the mean of the degrees is estimated as (1/n) * \\sum_i
  {w_i * d_i}}
}
\value{
  the dansum estimate of the hidden population's size (as a
  prevalence) broken down by the categories given
}
\description{
  this function needs is the degree estimates for the
  respondents, along with the *same* attributes for each
  respondent. for example, it requires a dataset like this
  to be passed in\cr \tabular{ccc}{ d.hat \tab age \tab sex
  \cr 150 \tab 20 \tab f \cr 163 \tab 44 \tab m \cr 110
  \tab 60 \tab m \cr }
}
\details{
  \itemize{ \item{TODO}{ eventually handle weights? this is
  hard, i think} \item{TODO}{ eventually handle degree
  ratios...} \item{TODO}{ eventually allow specific columns
  to be picked out of dataframe} \item{TODO}{ eventually
  handle factors and continuous vars...} \item{TODO}{ for
  now, we're assuming that if you there is a combination of
  attributes that results in a total number known of NA,
  then that should be 0 (ie, we're assuming no alters were
  reported with that combination of attributes). this means
  that it's currently important to handle missing data
  *before* calling this function} }
}
\examples{
## example 1
altdist <- alter.distn.estimator(resp.data=thisdat,
                                 attribute.names=these.att,
                                 d.hat.col="d.hat",
                                 dropmiss=FALSE)
}

