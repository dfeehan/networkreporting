\name{multiplicity.estimator}
\alias{multiplicity.estimator}
\title{multiplicity.estimator}
\usage{
  multiplicity.estimator(survey.data, mult.col = "mult")
}
\arguments{
  \item{survey.data}{the dataframe with the survey results}

  \item{mult.col}{the name or index of the column that
  contains, for each respondent, the individual value of
  the number known divided by the sum of the
  multiplicities}
}
\value{
  the multiplicity estimate of the hidden population's size
  (as a prevalence)
}
\description{
  compute multiplicity estimate of the population size
}

