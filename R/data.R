
##' Example household survey data
##'
##' Example of a household survey dataset, used in unit tests
##' and vignettes for the \code{networkreporting} package.
##'
##' @format A data frame with 2,406 rows and 36 variables:
##' \describe{
##'   \item{id}{a unique identifier}
##'   \item{cluster}{the cluster (part of the complex survey design)}
##'   \item{region}{the region (part of the complex survey design)}
##'   \item{indweight}{the individual weight (relative)}
##'   \item{sex}{the sex of the respondent}
##'   \item{age.cat}{the age category of the respondent}
##'   \item{...}{all other rows are reported number of connections to various groups}
##' }
"example.survey"

##' Example known population data
##'
##' Example of a household survey dataset, used in unit tests
##' and vignettes for the \code{networkreporting} package.
##'
##' @format A data frame with 22 rows and 2 variables:
##' \describe{
##'   \item{known.popn}{The name of the group}
##'   \item{size}{The number of people in the group}
##' }
"example.knownpop.dat"

