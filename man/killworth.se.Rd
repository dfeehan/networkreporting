% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/variance_estimators.r
\name{killworth.se}
\alias{killworth.se}
\title{killworth.se}
\usage{
killworth.se(
  estimates,
  d.hat,
  total.popn.size = NULL,
  total = TRUE,
  missing = "ignore"
)
}
\arguments{
\item{estimates}{TODO}

\item{d.hat}{TODO}

\item{total.popn.size}{TODO}

\item{total}{TODO}

\item{missing}{TODO}
}
\value{
the estimated standard error
}
\description{
compute standard errors for scale-up estimates
based on the Killworth estimator
}
\details{
note that this is provided for comparison, but
that we do not generally recommend using this
strategy for estimating variance
}
\keyword{internal}
