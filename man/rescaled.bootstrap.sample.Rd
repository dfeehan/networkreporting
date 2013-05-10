\name{rescaled.bootstrap.sample}
\alias{rescaled.bootstrap.sample}
\title{rescaled.bootstrap.sample}
\usage{
  rescaled.bootstrap.sample(survey.data, survey.design,
    parallel = FALSE, paropts = NULL, num.reps = 1)
}
\arguments{
  \item{survey.data}{the dataset to use}

  \item{survey.design}{a formula describing the design of
  the survey (see below - TODO)}

  \item{num.reps}{the number of bootstrap replication
  samples to draw}

  \item{parallel}{if TRUE, use parallelization (via
  \code{plyr})}

  \item{paropts}{an optional list of arguments passed on to
  \code{plyr} to control details of parallelization}
}
\value{
  a list with \code{num.reps} entries. each entry is a
  dataset which has at least the variables \code{index}
  (the row index of the original dataset that was
  resampled) and \code{weight.scale} (the factor by which
  to multiply the sampling weights in the original
  dataset).
}
\description{
  given a survey dataset and a description of the survey
  design (ie, which combination of vars determines primary
  sampling units, and which combination of vars determines
  strata), take a bunch of bootstrap samples for the
  rescaled bootstrap estimator (see, eg, Rust and Rao
  1996).
}
\details{
  Note that we assume that the formula uniquely specifies
  PSUs. This will always be true if the PSUs were selected
  without replacement. If they were selected with
  replacement, then it will be necessary to make each
  realization of a given PSU in the sample a unique id.
  Bottom line: the code below assumes that all observations
  within each PSU (as identified by the design formula) are
  from the same draw of the PSU.

  TODO -- need to handle the case where there are no PSUs
  (either SRS within strata or just SRS) (see note in code
  below) note that this should include handling the case
  where the design is something like ~ 1 \cr TODO -- need
  to test that this works in cases where the PSU design has
  more than one variable specified (see espsecially comment
  inline below)

  \code{survey.design} is a formula of the form\cr weight ~
  psu_vars + strata(strata_vars), where weight is the
  variable with the survey weights and psu is the variable
  denoting the primary sampling unit
}

