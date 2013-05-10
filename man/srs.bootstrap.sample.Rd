\name{srs.bootstrap.sample}
\alias{srs.bootstrap.sample}
\title{srs.bootstrap.sample}
\usage{
  srs.bootstrap.sample(survey.data, survey.design = NULL,
    parallel = FALSE, paropts = NULL, num.reps = 1)
}
\arguments{
  \item{survey.data}{the dataset to use}

  \item{num.reps}{the number of bootstrap replication
  samples to draw}

  \item{survey.design}{not used here, but included for
  compatability with other bootstrap resamplers}

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
  strata), take a bunch of bootstrap samples under a simple
  random sampling (with repetition) scheme
}
\details{
  \code{survey.design} is not needed; it's included as an
  argument to make it easier to drop
  \code{srs.bootstrap.sample} into the place of other
  bootstrap functions, which do require information about
  the survey design
}

