##########################################################
## test-kp.r
##
## unit tests for functions that compute known population
## degree estimates
##
## TODO -- eventually, develop a catalog of simple networks
##         that we can hand-compute estimator values for,
##         and that can be part of these tests
##         (see also the tests for test_estimators.r)

## TODO -- I don't understand why @import plyr,
## which is in the networkreporting-help.R file,
## doesn't take care of this...
library(plyr)

## these tests use the toy networks that come
## packaged with the networkreporting package
## TODO -- I don't understand why the package
## data aren't available without having to
## specify package=...
## (this could be a devtools thing?)
data(toynetworks,package="networkreporting")
data(toynrnetworks,package="networkreporting")

####################################
## known population estimator
context("estimators - known population")

## TODO
## NOTE that the toy networks used in the estimator tests
##      would also work here...

####################################
## total degree estimator
context("estimators - known population total degree")

## TODO
