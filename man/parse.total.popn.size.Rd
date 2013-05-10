\name{parse.total.popn.size}
\alias{parse.total.popn.size}
\title{handle the total.popn.size argument in a uniform way across
several functions}
\usage{
  parse.total.popn.size(total.popn.size, survey.data,
    verbose = TRUE)
}
\arguments{
  \item{total.popn.size}{value to parse}

  \item{survey.data}{the dataframe we're analyzing, which
  may or may not have an attribute called
  'total.popn.size'}

  \item{verbose}{if TRUE, print messages to the screen}
}
\value{
  a vector whose entries have various summaries of fit
}
\description{
  handle the total.popn.size argument in a uniform way
  across several functions, including
  \code{\link{kp.degree.estimator}},
  \code{\link{nsum.internal.validation}}, and
  \code{\link{nsum.estimator}}.
}
\keyword{internal}

