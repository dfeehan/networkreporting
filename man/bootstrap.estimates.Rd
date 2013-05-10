\name{bootstrap.estimates}
\alias{bootstrap.estimates}
\title{bootstrap.estimates}
\usage{
  bootstrap.estimates(survey.data, survey.design,
    bootstrap.fn, estimator.fn, num.reps, weights = NULL,
    ..., summary.fn = NULL, verbose = TRUE,
    parallel = FALSE, paropts = NULL)
}
\arguments{
  \item{survey.data}{the dataset to use}

  \item{survey.design}{a formula describing the design of
  the survey (see below - TODO)}

  \item{estimator.fn}{name of a function which, given a
  dataset like survey data and arguments in \code{...},
  will produce an estimate of interest}

  \item{bootstrap.fn}{name of the method to be used to take
  bootstrap resamples; see below}

  \item{num.reps}{the number of bootstrap replication
  samples to draw}

  \item{weights}{weights to use in estimation (or NULL, if
  none)}

  \item{summary.fn}{an optional function which, given the
  set of estimates produced by estimator.fn, summarizes
  them}

  \item{parallel}{if TRUE, use the plyr library's .parallel
  argument to produce bootstrap resamples and estimates in
  parallel}

  \item{paropts}{if not NULL, additional arguments to pass
  along to the parallelization routine}

  \item{verbose}{if TRUE, produce lots of feedback about
  what is going on}

  \item{...}{additional arguments which will be passed on
  to the estimator fn}
}
\value{
  if no summary.fn is specified, then return the list of
  estimates produced by estimator.fn; if summary.fn is
  specified, then return its output
}
\description{
  rescaled bootstrap method\cr this function contains the
  core of the rescaled bootstrap method for estimating
  uncertainty in our estimates it should be designed so
  that it can be passed in to estimation functions as an
  argument\cr OR\cr \cr TODO -- estimator.fn/bootstrap.fn
  and summary.fn are treated differently (one expects
  characters, one expects an actual fn. fix!)\cr \cr TODO
  -- write description block, including estimator.fn,
  bootstrap.fn, summary.fn, more? \cr TODO -- should check
  that estimator fn takes all the arguments it is supposed
  to (for example, it needs to take a 'weights' argument
  and a 'survey.data' argument)\cr TODO -- write example
  usage code for documentation...
}
\examples{
\donttest{
# code goes here
...
}
}

