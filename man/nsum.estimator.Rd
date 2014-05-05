\name{nsum.estimator}
\alias{nsum.estimator}
\title{nsum.estimator}
\usage{
  nsum.estimator(survey.data, d.hat.vals = "d",
    y.vals = "y", total.popn.size = NULL, deg.ratio = 1,
    tx.rate = 1, weights = NULL, killworth.se = FALSE,
    missing = "ignore", verbose = FALSE, ...)
}
\arguments{
  \item{survey.data}{the dataframe with survey results}

  \item{d.hat.vals}{the name or index of the column that
  contains each respondent's estimated degree}

  \item{y.vals}{the name or index of the column that
  contains the count of hidden popn members known}

  \item{total.popn.size}{NULL, NA, or a size}

  \item{weights}{if not NULL, weights to use in computing
  the estimate. this should be the name of the column in
  the survey.data which has the variable with the
  appropriate weights. these weights should be construted
  so that, eg, the mean of the degrees is estimated as
  (1/n) * \\sum_i {w_i * d_i}}

  \item{deg.ratio}{the degree ratio,
  \\frac{\\bar{d_T}}{\\bar{d}}; defaults to 1}

  \item{tx.rate}{the information transmission rate;
  defaults to 1}

  \item{killworth.se}{if not NA, return the Killworth et al
  estimate of}

  \item{missing}{if "ignore", then proceed with the
  analysis without doing anything about missing values. if
  "complete.obs" then only use rows that have no
  missingness for the computations (listwise deletion).
  care must be taken in using this second option}

  \item{verbose}{if TRUE, print messages to the screen}

  \item{...}{extra parameters to pass on to the bootstrap
  fn, if applicable}
}
\value{
  the nsum estimate of the hidden population's size (as a
  prevalence or an absolute number, depending on
  total.popn.size)
}
\description{
  compute network scale-up (nsum) estimate of the hidden
  population's size. if the degree ratio and information
  transmission rate are both 1 (the defaults), this is the
  Killworth estimator (Killworth et al 1998, "A social
  network approach...")
}
\examples{
data(hhsurvey)
 kp.vec <- df.to.kpvec(knownpop.dat, kp.var='known.popn', kp.value='size')
 example.survey <- add.kp(example.survey, kp.vec)
 d.hat <- kp.degree.estimator(example.survey,
                              missing="complete.obs",
                              total.popn.size=10e6)
 example.survey$d.hat <- d.hat
 sw.estimate <- nsum.estimator(example.survey,
                               d.hat.vals="d.hat",
                               y.vals="sex.workers",
                               weights="indweight",
                               missing="complete.obs",
                               total.popn.size=10e6)
}

