\name{gwsm.estimator}
\alias{gwsm.estimator}
\title{indirect estimator
(generalized weight share method / gwsm)}
\usage{
  gwsm.estimator(survey.data, gwsm.col = "mult")
}
\arguments{
  \item{survey.data}{the dataframe with the survey results}

  \item{gwsm.col}{the name or index of the column that
  contains, for each respondent, the individual value of
  the number known divided by the sum of the
  multiplicities}
}
\value{
  the multiplicity estimate of the hidden population's size
  (as a prevalence)
}
\description{
  compute gwsm estimate of the population size
}

