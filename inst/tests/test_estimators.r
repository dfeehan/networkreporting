##########################################################
## test-estimators.R
##
## unit tests for functions that compute estimators
## from a dataset
##
## TODO -- eventually, develop a catalog of simple networks
##         that we can hand-compute estimator values for,
##         and that can be part of these tests

## NB: for each estimator, we should test the following:
##     * for one or more specific datasets, test that the
##       estimator return the (known) right answer
##
##     * [NOT IMPLEMENTED YET] test the accuracy of the
##       leave-one-out validation procedure
##
##     * [NOT IMPLEMENTED YET] for the same datasets,
##       test that the weighted versions of the estimator
##       returns the (known) right answer
##
##     * [NOT IMPLEMENTED YET] test that NAs are handled
##       in weighted and unweighted estimators
##
##     * test error handling in passing in and using
##       column names
##
##     * [NOT IMPLEMENTED YET] test the other ways
##       of passing in and using column names
##
##     * [NOT IMPLEMENTED YET] test the variance
##       estimators (this will likely be several tests)

## these tests use the toy networks that come
## packaged with the networkreporting package
## TODO -- I don't understand why the package
## data aren't available without having to
## specify package=...
## (this could be a devtools thing?)
data(toynetworks,package="networkreporting")
data(toynrnetworks,package="networkreporting")

####################################
## multiplicity estimator
context("estimators - multiplicity")

ests <- ldply(toy.networks,
              function(tn) {
                return(c("est"=multiplicity.estimator(tn,
                                                      mult.col="mult.response")))
              })
truth <- ldply(toy.networks,
               function(tn) { c("truth"=attr(tn, "multiplicity.estimate")) })

tocheck <- merge(ests,
                 truth,
                 by=".id",
                 all=TRUE)

d_ply(tocheck,
      .(.id),
      function(x) {
        expect_that(x$est, equals(x$truth),
                    info=paste("estimate is", x$est,
                               "but it should be", x$truth),
                    label=paste("multiplicity estimate on toy network ",
                                x$".id"))
      })


####################################
## nsum estimator
context("estimators - nsum")

## TODO - add test for Killworth estimate of the
##        standard error (both for proportions and for totals)

ests <- ldply(toy.networks,
              function(tn) {
                return(data.frame(nsum.estimator(tn,
                                                 d.hat.col="d",
                                                 y.col="y",
                                                 total.popn.size=NA)))
              })
truth <- ldply(toy.networks,
               function(tn) { c("truth"=attr(tn, "nsum.estimate")) })

tocheck <- merge(ests,
                 truth,
                 by=".id",
                 all=TRUE)

d_ply(tocheck,
      .(.id),
      function(x) {
        expect_that(x$estimate, equals(x$truth),
                    info=paste("estimate is", x$est,
                               "but it should be", x$truth),
                    label=paste("nsum estimate on toy network ",
                                x$".id"))
      })

## TODO -- we should also check the numerators and the denominators
## TODO -- add weights and check that weighted estimates work properly
##         as well


####################################
## gnsum estimator
context("estimators - gnsum")

ests <- ldply(toy.networks,
              function(tn) {
                d.T.bar <- mean(subset(tn,hidden)$d)
                d.bar <- mean(tn$d)
                delta <- d.T.bar/d.bar

                return(data.frame(nsum.estimator(tn,
                                                 d.hat.col="d",
                                                 y.col="y",
                                                 deg.ratio=delta,
                                                 total.popn.size=NA)))
              })
truth <- ldply(toy.networks,
               function(tn) { c("truth"=attr(tn, "gnsum.estimate")) })

tocheck <- merge(ests,
                 truth,
                 by=".id",
                 all=TRUE)

d_ply(tocheck,
      .(.id),
      function(x) {
        expect_that(x$est, equals(x$truth),
                    info=paste("estimate is", x$est,
                               "but it should be", x$truth),
                    label=paste("gnsum estimate on toy network ",
                                x$".id"))
      })

####################################
## network reporting estimator
##context("estimators - network reporting")

## TODO (NB: see README from non-versioned networkreporting folder)
