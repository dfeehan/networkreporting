\name{add.kp}
\alias{add.kp}
\title{attach known populations to a dataframe}
\usage{
  add.kp(survey.data, kp.vec, total.pop.size = NULL)
}
\arguments{
  \item{survey.data}{the survey dataframe}

  \item{kp.vec}{the known population vector}

  \item{total.pop.size}{(optional) the total population
  size to use (see below)}
}
\value{
  the survey dataframe with the known population vector
  attached as an attribute
}
\description{
  take a known population vector (see
  \code{\link{df.to.kpvec}}) and associate it with a survey
  dataframe. this makes it more convenient to use some of
  the \code{networkreporting} package's function
}
\details{
  The \code{total.popn.size} parameter is interpreted as
  follows: \itemize{ \item NA if total.popn.size is NA then
  work with proportions \item NULL if total.popn.size is
  NULL (nothing passed in), then assume that there's a
  total.popn.size attribute associated with the dataset
  we're using \item numerical value if an actual
  total.popn.size was passed in, use that value }
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
  \link{df.to.kpvec}
}

