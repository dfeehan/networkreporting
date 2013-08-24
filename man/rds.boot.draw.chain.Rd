\name{rds.boot.draw.chain}
\alias{rds.boot.draw.chain}
\title{draw RDS bootstrap resamples for one chain}
\usage{
  rds.boot.draw.chain(chain, mm, dd, parent.trait,
    idvar = "uid")
}
\arguments{
  \item{chain}{the chain to draw resamples for}

  \item{mm}{the mixing model to use}

  \item{dd}{the degree distns to use}

  \item{parent.trait}{a vector whose length is the number
  of bootstrap reps we want}

  \item{idvar}{the name of the variable used to label the
  columns of the output (presumably some id identifying the
  row in the original dataset they come from -- see below)}
}
\value{
  a list with two entries \itemize{ \item trait the draws
  for the trait variable \item degree the draws for the
  degrees (conditional on the drawn } each entry is a
  dataset whose columns are the name of the idvar specified
  in \code{idvar} and the value of that id, concatenated
  together with a '.'
}
\description{
  this function uses the algorithm described in the
  supporting online material for Weir et al 2012 (TODO
  PROPER CITE) to take bootstrap resamples of one chain
  from an RDS dataset
}

