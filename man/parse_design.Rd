\name{parse_design}
\alias{parse_design}
\title{parse a formula that describes the design of a survey}
\usage{
  parse_design(formula)
}
\arguments{
  \item{formula}{a formula describing the sample design
  (see above)}
}
\value{
  a list with entries \code{psu.formula} and
  \code{strata.formula}
}
\description{
  Given a formula of the form\cr \code{~ psu_v1 + psu_v2 +
  ... + strata(strata_v1 + strata_v2 + ...)}\cr \itemize{
  \item{"psu.formula"}{a formula describing the primary
  sampling unit vars} \item{"strata.formula"}{a formula
  describing the strata (if any)} }\cr
}
\keyword{internal}

