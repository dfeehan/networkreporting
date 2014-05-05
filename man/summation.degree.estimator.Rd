\name{summation.degree.estimator}
\alias{summation.degree.estimator}
\title{summation.degree.estimator}
\usage{
  summation.degree.estimator(survey.data, sum.q = NULL,
    missing = "ignore")
}
\arguments{
  \item{survey.data}{the dataframe with the survey results}

  \item{sum.q}{if not NULL, a vector whose entries are the
  variable names in the dataset corresponding to each
  summation question. if NULL, then assume that the
  survey.data dataframe has an attribute called 'sum.qs'
  containing this vector.}

  \item{missing}{if "ignore", then proceed with the
  analysis without doing anything about missing values.
  other options are not yet implemented.}
}
\value{
  a vector with an estimate of the degree for each row in
  survey.data. if na.rm=TRUE, then the degree for rows that
  have missingness in the summation questions will be set
  to NA
}
\description{
  compute an estimate of the respondents' degrees using the
  summation method (McCarty et al 2001)
}
\details{
  This is the method first described in McCarty et al 2001,
  "Comparing two methods for estimating network size"

  Note that the summation degree estimator for the case
  where there is missing data is not yet implemented. (In
  fact, I don't think that there is a known estimator for
  this case.)
}

