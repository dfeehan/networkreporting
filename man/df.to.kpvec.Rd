\name{df.to.kpvec}
\alias{df.to.kpvec}
\title{turn a dataframe into a known population vector}
\usage{
  df.to.kpvec(kp.data, kp.var, kp.value)
}
\arguments{
  \item{kp.data}{the known population dataset}

  \item{kp.var}{the column of \code{kp.data} that has known
  population names; either a column name, a column index,
  or a vector of values}

  \item{kp.value}{the column of \code{kp.data} that has
  known population sizes; either a column name, a column
  index, or a vector of value}
}
\value{
  a vector whose entries have the known population values
  and whose names have the corresponding \code{kp.var}
  value
}
\description{
  \code{df.to.kpvec} takes a dataframe which has a column
  with known population names, and a column with known
  population totals, and turns it into a known population
  vector. if the names of the survey variables
  corresponding to each known population are available,
  they can be passed in as well
}
\examples{
\dontrun{
  # if kp.dat is a dataframe with columns 'kp' with known popn names
  # and 'total.size' with the total size,
  # and my.survey is the dataframe with survey responses

  my.kp.vec <- df.to.kpvec(kp.data, kp.var='kp', kp.value='total.size')
  my.survey <- add.kp(my.survey, my.kp.vec)

  # now we can call estimator functions like
  # kp.degree.estimator without having to specify known
  # populations each time
}
}
\seealso{
  \link{add.kp}
}

