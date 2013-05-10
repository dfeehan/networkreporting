\name{sum.degree.estimator}
\alias{sum.degree.estimator}
\title{sum.degree.estimator}
\usage{
  sum.degree.estimator(survey.data, sum.q = NULL,
    na.rm = FALSE)
}
\arguments{
  \item{survey.data}{the dataframe with the survey results}

  \item{sum.q}{if not NULL, a vector whose entries are the
  variable names in the dataset corresponding to each
  summation question. if NULL, then assume that the
  survey.data dataframe has an attribute called 'sum.qs'
  containing this vector.}

  \item{na.rm}{if TRUE, disregard rows that have any
  missingness in the summation populations; otherwise, use
  an adjusted estimator to produce those rows' degree
  estimates}
}
\value{
  a vector with an estimate of the degree for each row in
  survey.data. if na.rm=TRUE, then the degree for rows that
  have missingness in the summation questions will be set
  to NA
}
\description{
  compute an estimate of the respondents' degrees using the
  summation method
}
\details{
  Note that the summation degree estimator for the case
  where there is missing data is not yet implemented. (In
  fact, I don't think that there is a known estimator for
  this case.)
}

