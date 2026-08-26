##' Network reporting estimators
##'
##' \code{networkreporting} has methods for analyzing data that were collected
##' using network reporting techniques. It includes estimators appropriate for
##' indirect sampling, network scale-up, network reporting, and sibling history
##' methods.
##'
##' @aliases package-networkreporting
##' @import reshape2 functional ggplot2 dplyr lazyeval surveybootstrap
##' @useDynLib networkreporting, .registration = TRUE
##' @importFrom Rcpp sourceCpp
"_PACKAGE"

##' @importFrom stats setNames quantile sd weighted.mean
##' @importFrom rlang sym :=

##' @importFrom plyr aaply
##' @importFrom plyr ldply
##' @importFrom plyr dlply
##' @importFrom plyr llply
##' @importFrom plyr laply
##' @importFrom plyr alply
##' @importFrom plyr colwise
##' @importFrom plyr d_ply
##' @importFrom plyr l_ply
##' @importFrom plyr join
##' @importFrom plyr .
NULL

##' @importFrom stringr str_match
##' @importFrom stringr str_c
##' @importFrom stringr str_locate
##' @importFrom stringr str_split
NULL
