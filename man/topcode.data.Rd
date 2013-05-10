\name{topcode.data}
\alias{topcode.data}
\title{topcode a group of variables}
\usage{
  topcode.data(survey.data, vars, max, to.na = NULL,
    ignore = NA)
}
\arguments{
  \item{survey.data}{the dataset with the survey responses}

  \item{vars}{a vector with the names or indices of the
  columns in the dataframe that are to be topcoded}

  \item{max}{the maximum value; all values > max are
  recoded to max}

  \item{ignore}{a vector of values to leave unchanged}

  \item{to.na}{a vector of values to recode to NA (this
  happens before topcoding)}
}
\value{
  the topcoded vector
}
\description{
  TODO describe
}
\examples{
\dontrun{
   ## write examples
}
}

