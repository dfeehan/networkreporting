\name{networkreporting.estimator}
\alias{networkreporting.estimator}
\title{networkreporting.estimator}
\usage{
  networkreporting.estimator(resp.data, attribute.data,
    attribute.names = NULL, resp.attribute.names = NULL,
    d.hat.vals = NULL, weights = NULL,
    attribute.weights = NULL)
}
\arguments{
  \item{resp.data}{the dataframe that has the estimated
  degree of each respondent, along with the same
  characteristics reported for each alter that are found in
  attribute.data. note that the estimated degree should be
  in the column indicated by d.hat.col, and that the column
  names of the attributes should match their names in
  attribute.data}

  \item{attribute.data}{the long-form dataframe with
  reported attributes of alters in the subgp we're
  studying.}

  \item{attribute.names}{if not NULL, the names of the
  columns of attribute.data and resp.data that contain the
  attribute information.  if NULL, then all of the columns
  of attribute.data are assumed to have attribute
  information}

  \item{resp.attribute.names}{if not NULL, the names of the
  columns of the respondent data that contain the
  respondent's attributes.  these should correspond to the
  entries in attribute.names.  if NULL, then the names are
  taken to be the same as attribute.names}

  \item{d.hat.vals}{the name or index of the column with
  the degree estimates in resp.data; all the other columns
  are assumed to be attributes}

  \item{weights}{if not NULL, weights to use in computing
  the estimate. this should be the name of the column in
  the resp.data which has the variable with the appropriate
  weights. these weights should be construted so that, eg,
  the mean of the degrees is estimated as (1/n) * \\sum_i
  {w_i * d_i}}

  \item{attribute.weights}{the weights for the attribute
  variable. if NULL, assume these are the same as weights}
}
\value{
  the network reporting estimate of the hidden population's
  size (as a prevalence) broken down by the categories
  given
}
\description{
  compute the network reporting estimate of the hidden
  population's size. this function takes two sources of
  data as input: first, it requires a long-form dataframe
  with the attributes of the reported members of the hidden
  population. for example, if we are asking about emigres
  and we collect the age and sex of each reported emigrant,
  then the long form dataset might look like \tabular{cc}{
  age \tab sex\cr 15 \tab m\cr 58 \tab f\cr 33 \tab m\cr }
  note that, in this dataset, we do not need to know which
  respondent reported each row. (this may change once we
  work out weighting, et cetera).  the second source of
  data we need is the degree estimates for the respondents,
  along with the *same* attributes for each respondent. for
  example, in the situation above, we would also require a
  dataset like this to be passed in \tabular{ccc}{ d.hat
  \tab age \tab sex\cr 150 \tab 20 \tab f\cr 163 \tab 44
  \tab m\cr 110 \tab 60 \tab m\cr }\cr TODO - eventually
  handle weights? this is hard, i think. also be sure to
  use the get.weights helper function instead of the
  weights.col argument used below\cr TODO - more
  housekeeping to check the two data sets agree (see
  comment inline, below)\cr TODO - eventually handle degree
  ratios...\cr TODO - eventually allow specific columns to
  be picked out of dataframe\cr TODO - eventually handle
  factors and continuous vars...\cr TODO - for now, we're
  assuming that if you there is a combination of attributes
  that results in a total number known of NA, then that
  should be 0 (ie, we're assuming no alters were reported
  with that combination of attributes). this means that
  it's currently important to handle missing data *before*
  calling this function
}

