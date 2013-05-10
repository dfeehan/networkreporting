\name{attributes.to.long}
\alias{attributes.to.long}
\title{attributes.to.long}
\usage{
  attributes.to.long(data, attribute.prefix,
    ego.vars = NULL, keep.na = FALSE, idvar = NULL,
    regexp = "^(.+)([\\.|_])(\\d+$)", regexp.index.gp = 3,
    regexp.vname.gp = 1)
}
\arguments{
  \item{data}{the wide-form dataset to convert}

  \item{attribute.prefix}{a vector whose entries have the
  prefixes of the names of variables in the dataframe
  \code{data} that pertain to each alter. if you'd like
  these to be re-named in the long form data, then the
  variable names you'd like to use in the long form should
  be the names of each entry in this vector. in the example
  above, we would use
  \code{attribute.prefix=c("age"="emage", "sex"="emsex")}.
  see \code{regexp}, below, to understand how these
  prefixes are used to match columns of the dataset; by
  default, we assume that the variables match
  <prefix><either '.' or '_'><number>.}

  \item{ego.vars}{if not NULL, the names of columns in the
  dataset that refer to the egos and so should not be
  converted to long-form. you can specify that they should
  be renamed in the same way as with
  \code{attribute.prefix}. in the example above, we would
  use \code{ego.vars=c("degree"="resp.d.hat")}.}

  \item{keep.na}{if FALSE, remove columns in the resulting
  dataset that are all NAs}

  \item{idvar}{the index or name of the variable in the
  data that has the respondent id. if NULL, then new ids
  which correspond to the rows in data are created.}

  \item{regexp}{the regular expression which describes the
  wide-form variable names the default is anything that
  ends in a "." or a "_" and a number.  if you specify your
  own regular expression, it should use groups (specified
  with "(" and ")"). the default is
  \code{"^(.+)([\\.|_])(\\d+$)"}, which specifies three
  groups (see below for how these groups are used).}

  \item{regexp.index.gp}{the group(s) in regexp which
  should match the wide-form variable name prefixes
  specified in \code{attribute.prefix}. in the default,
  this is the first group, so \code{regexp.index.gp=1}.}

  \item{regexp.vname.gp}{the group(s) in regexp which
  should vary over the different alters; in the default,
  this is the third group, so \code{regexp.vname.gp=3}.}
}
\value{
  a long-form dataframe with the attributes reported for
  all of the alters.  the dataframe will include an
  alter.id variable which is formed using <respondent
  id>.<alter number>
}
\description{
  Start with a wide-form dataframe reported about alters
  using network method questions and convert it into a
  long-form dataset. For example, after a network survey of
  out-migrants, there might be variables about sex and age
  of each emigre reported to be connected to each
  respondent. In a study that encountered a maximum of 3
  reported emigres across all respondents, this wide-form
  dataframe might look like:\cr \tabular{ccccccccc}{
  resp.id\tab resp.d.hat\tab emage.1\tab emage.2\tab
  emage.3\tab emsex.1\tab emsex.2\tab emsex.3\cr 1\tab
  100\tab 24\tab NA\tab NA\tab M\tab NA\tab NA\cr 2\tab
  110\tab NA\tab NA\tab NA\tab NA\tab NA\tab NA\cr 3\tab
  140\tab 33\tab 23\tab 53\tab F\tab M\tab F\cr ... \cr }
  The \code{attributes.to.long} function could convert that
  into a long-form dataframe that looks like this:\cr
  \tabular{ccc}{ degree\tab age\tab sex\cr 100\tab 24\tab
  M\cr 140\tab 33\tab F\cr 140\tab 23\tab M\cr 140\tab
  53\tab F\cr \tab...\tab\cr } (Note that we make no
  guarantees about the order in which the reshaped data
  will be returned.)\cr \itemize{ \item{TODO - }{for now,
  this converts any factors into characters.  this is
  obviously not ideal. eventually, data type should be
  preserved...} \item{TODO - }{handle the case of "" more
  effectively. Right now, we *assume* that all structural
  missings (eg, I only report one alter, though there are
  three columns for me to do so) are NA} \item{TODO -
  }{look at the code in the middle of the function that's
  commented out and be sure we know that the order of the
  rows will be the same, to that we can cbind them
  together.} }
}
\examples{
\dontrun{
   ## TODO add example
}
}

