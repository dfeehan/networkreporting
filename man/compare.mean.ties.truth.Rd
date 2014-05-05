\name{compare.mean.ties.truth}
\alias{compare.mean.ties.truth}
\title{compare.mean.ties.truth}
\usage{
  compare.mean.ties.truth(survey.data, weights = NULL,
    known.popns = NULL)
}
\arguments{
  \item{survey.data}{the dataframe with the survey results}

  \item{known.popns}{if not NULL, a vector whose entries
  are the size of the known populations, and whose names
  are the variable names in the dataset corresponding to
  each one. if NULL, then assume that the survey.data
  dataframe has an attribute called 'known.popns'
  containing this vector.}

  \item{weights}{if not NULL, weights to use in computing
  the estimate. this should be the name of the column in
  the survey.data which has the variable with the
  appropriate weights. these weights should be construted
  so that, eg, the mean of the degrees is estimated as
  (1/n) * \\sum_i {w_i * d_i}}
}
\value{
  a ggplot2 object with the relationship plot
}
\description{
  plot the relationship between the mean number of ties in
  the survey dataset and the true popn sizes
}

