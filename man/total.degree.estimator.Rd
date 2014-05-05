\name{total.degree.estimator}
\alias{total.degree.estimator}
\title{total.degree}
\usage{
  total.degree.estimator(survey.data, d.hat.vals = "d",
    weights = NULL, missing = "ignore")
}
\arguments{
  \item{survey.data}{the dataframe with survey results}

  \item{d.hat.vals}{the name or index of the column that
  contains each respondent's estimated degree}

  \item{weights}{if not NULL, weights to use in computing
  the estimate. this should be the name of the column in
  the survey.data which has the variable with the
  appropriate weights. these weights should be construted
  so that, eg, the mean of the degrees is estimated as
  (1/n) * \\sum_i {w_i * d_i}}

  \item{missing}{if "ignore", then proceed with the
  analysis without doing anything about missing values. if
  "complete.obs" then only use rows that have no
  missingness for the computations (listwise deletion).
  care must be taken in using this second option}
}
\value{
  the estimated total degree
}
\description{
  Estimate the total degree of the population network from
  sample degrees
}
\details{
  This computes the weighted sum of the respondents'
  estimated degrees. For now, this function doesn't worry
  about missing values OR about differences between the
  frame and the universe.
}
\examples{
data(hhsurvey)
 kp.vec <- df.to.kpvec(knownpop.dat, kp.var='known.popn', kp.value='size')
 example.survey <- add.kp(example.survey, kp.vec)
 d.hat <- kp.degree.estimator(example.survey,
                              missing="complete.obs",
                              total.popn.size=NA)
 example.survey$d.hat <- d.hat
 tot.d.hat <- total.degree.estimator(example.survey,
                                     d.hat.vals="d.hat",
                                     weights="indweight",
                                     missing="complete.obs")
}

