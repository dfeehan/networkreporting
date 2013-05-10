\name{kp.degree.estimator}
\alias{kp.degree.estimator}
\title{kp.degree.estimator}
\usage{
  kp.degree.estimator(survey.data, known.popns = NULL,
    total.popn.size = NULL, weights = NULL, na.rm = FALSE,
    verbose = TRUE)
}
\arguments{
  \item{survey.data}{the dataframe with the survey results}

  \item{known.popns}{if not NULL, a vector whose entries
  are the size of the known populations, and whose names
  are the variable names in the dataset corresponding to
  each one. if NULL, then assume that the survey.data
  dataframe has an attribute called 'known.popns'
  containing this vector.}

  \item{total.popn.size}{the size of the entire population.
  if NULL, this function returns proportions; if not NULL,
  it returns absolute numbers (ie, the proportions * total
  popn size)}

  \item{weights}{if not NULL, weights to use in computing
  the estimate. this should be the name of the column in
  the survey.data which has the variable with the
  appropriate weights. these weights should be construted
  so that, eg, the mean of the degrees is estimated as
  (1/n) * \\sum_i {w_i * d_i}}

  \item{na.rm}{if TRUE, disregard rows that have any
  missingness in the known populations; otherwise, use an
  adjusted estimator to produce those rows' degree
  estimates}

  \item{verbose}{if TRUE, print messages to the screen}
}
\value{
  a vector with an estimate of the degree for each row in
  survey.data. if na.rm=TRUE, then the degree for rows that
  have missingness in the 'how many X' questions will be
  set to NA
}
\description{
  compute an estimate of the respondents' degrees using the
  known population method\cr
}

