\name{kp.degree.estimator}
\alias{kp.degree.estimator}
\title{kp.degree.estimator}
\usage{
  kp.degree.estimator(survey.data, known.popns = NULL,
    total.popn.size = NULL, missing = "ignore",
    verbose = FALSE)
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

  \item{missing}{if "ignore", then proceed with the
  analysis without doing anything about missing values. if
  "complete.obs" then, for each row, use only the known
  populations that have no missingness for the
  computations. care must be taken in using this second
  option}

  \item{verbose}{if TRUE, print messages to the screen}
}
\value{
  a vector with an estimate of the degree for each row in
  survey.data. if missing=="ignore", then the degree for
  rows that have missingness in the 'how many X' questions
  will be set to NA
}
\description{
  compute an estimate of the respondents' degrees using the
  known population method\cr
}
\details{
  note that this function does not take survey weights,
  since these estimates are not for total degree, but just
  for the individual degree of each respondent
}
\examples{
data(hhsurvey)
 kp.vec <- df.to.kpvec(knownpop.dat, kp.var='known.popn', kp.value='size')
 example.survey <- add.kp(example.survey, kp.vec)
 d.hat <- kp.degree.estimator(example.survey,
                              missing="complete.obs",
                              total.popn.size=NA)
}

