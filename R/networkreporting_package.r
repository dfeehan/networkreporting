##' Network reporting estimators
##'
##' \code{networkreporting} has methods for analyzing data that were collected
##' using network reporting techniques. It includes estimators appropriate for
##' indirect sampling, network scale-up, network reporting, and sibling history
##' methods.
##'
##' @docType package
##' @name networkreporting
##' @aliases networkreporting package-networkreporting
##' @import plyr reshape2 stringr functional ggplot2
NULL

################################################
## hhsurvey.RData
################################################

##' Example household survey data
##'
##' Example of a household survey dataset, used in unit tests
##' and vignettes for the \code{networkreporting} package.
##'
##' @name example.survey
##' @docType data
NULL

##' Names of ARD questions for known populations in example household survey data
##'
##' A vector containing the names of the columns of 
##' which correspond to aggregate relational data (ARD) questions
##' asked about groups of known size in \code{hhsurvey.data}
##'
##' @name hm.q
##' @docType data
NULL

##' Names of ARD questions for hidden populations in example household survey data
##'
##' A vector containing the names of the columns of 
##' which correspond to aggregate relational data (ARD) questions
##' asked about hidden groups (ie, groups of unknown size) in \code{hhsurvey.data}
##'
##' @name target.q
##' @docType data
NULL

##' Size of example known populations
##'
##' A dataframe with the size of the known populations corresponding
##' to the survey data in \code{hhsurvey.data}
##'
##' @name knownpop.dat
##' @docType data
NULL

##' Size of example total population
##'
##' A numeric with the size of the total population to be used
##' with the survey data in \code{hhsurvey.data}
##'
##' @name tot.pop.size
##' @docType data
NULL

################################################
## toynetwokrs.RData
################################################

##' Toy network data
##'
##' List with 4 toy network examples, useful for unit tests in the
##' \code{networkreporting} package
##'
##' @name toy.networks
##' @docType data
NULL

##' Toy network example number 1
##'
##' Toy network example, useful for unit tests in the
##' \code{networkreporting} package
##'
##' @name tn1
##' @docType data
NULL

##' Toy network example number 2
##'
##' Toy network example, useful for unit tests in the
##' \code{networkreporting} package
##'
##' @name tn2
##' @docType data
NULL

##' Toy network example number 3
##'
##' Toy network example, useful for unit tests in the
##' \code{networkreporting} package
##'
##' @name tn3
##' @docType data
NULL

##' Toy network example number 4
##'
##' Toy network example, useful for unit tests in the
##' \code{networkreporting} package
##'
##' @name tn4
##' @docType data
NULL

################################################
## toynrnetworks.RData
################################################

##' Toy network response networks
##'
##' List with 3 toy network examples with attributes attached to each
##' node; useful for unit tests in the \code{networkreporting}
##' package.
##'
##' @name toy.nr.networks
##' @docType data
NULL

##' Toy network with attributes, example number 1
##'
##' Toy network example, useful for unit tests in the
##' \code{networkreporting} package
##'
##' @name tnr1
##' @docType data
NULL

##' Toy network with attributes, example number 2
##'
##' Toy network example, useful for unit tests in the
##' \code{networkreporting} package
##'
##' @name tnr2
##' @docType data
NULL

##' Toy network with attributes, example number 3
##'
##' Toy network example, useful for unit tests in the
##' \code{networkreporting} package
##'
##' @name tnr3
##' @docType data
NULL

################################################
## goc.RData
################################################

##' Example game of contacts parents (nominators)
##'
##' Example of a game of contacts dataset, collected via RDS,
##' organized from the perspective of the parents (ie, those who nominated
##' subsequent respondents). See also \code{survey.data}.
##' Used in unit tests and vignettes for the \code{networkreporting} package.
##'
##' @name parent.data
##' @docType data
NULL

##' Example game of contacts survey data
##'
##' Example of a game of contacts survey dataset, collected via RDS.
##' See also \code{parent.data}.
##' Used in unit tests and vignettes for the \code{networkreporting} package.
##'
##' @name survey.data
##' @docType data
NULL

################################################
## mu284.RData
################################################

##' MU284 population
##'
##' Data used in unit tests for variance estimation.
##' See TODO-Sarndal TODO-sampling package
##' TODO-doc describing unit tests
##'
##' @name MU284
##' @docType data
NULL

##' MU284 bootstrap results 
##'
##' list of datasets, each with one bootstrap sample
##' of the MU284 population; this is used in unit tests
##'
##' @name MU284.surveys
##' @docType data
NULL


##' MU284 bootstrap summary results 
##'
##' summary of the MU284 bootstrap resamples,
##' used in unit tests
##'
##' @name MU284.boot.res.summ
##' @docType data
NULL

##' function used in unit tests for MU284 bootstrap resamples
##'
##' this function is used as part of the unit tests for the
##' bootstrap resampling code
##'
##' @name MU284.estimator.fn
##' @docType data
NULL

