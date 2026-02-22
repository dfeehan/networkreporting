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
## packaged with the networksampling package
## TODO -- I don't understand why the package
## data aren't available without having to
## specify package=...
## (this could be a devtools thing?)
load("toynetworks.RData")
load("toynrnetworks.RData")

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
context("estimators - network survival")

## TODO (NB: see README from non-versioned networksampling folder)

## NB: for now, we're just testing the first network, since
##     the others have missing values, which we're not handling yet
#tocheck <- llply(1:length(toy.nr.networks),
tocheck <- plyr::llply(c(1),
              function(this.nrnet.idx) {

                this.nrnet <- toy.nr.networks[[this.nrnet.idx]]

                ## no weights in toy network data, so give everyone a weight
                ## of 1 for now
                this.nrnet$weight <- 1

                this.attrib <- toy.nr.long.networks[[this.nrnet.idx]]
                this.attrib$ego.weight <- 1

                netsurv.est <- network.survival.estimator_(resp.data=this.nrnet,
                                                           attribute.data=this.attrib,
                                                           attribute.names=c("age", "sex"),
                                                           known.populations="d",
                                                           weights="weight",
                                                           attribute.weights="ego.weight",
                                                           total.kp.size=nrow(this.nrnet))

                truth <- attr(this.nrnet, "ns.estimate")
                truth <- plyr::rename(truth, c('est'='truth'))

                netsurv.est <- dplyr::left_join(netsurv.est, truth,
                                                by=c('age', 'sex'))

                netsurv.est$nrid <- this.nrnet.idx

                return(netsurv.est)
              })

l_ply(tocheck,
      function(x) {
        expect_that(x$asdr.hat, equals(x$truth),
                    info=paste("estimate is", paste(x$asdr.hat, collapse=""),
                               "but it should be", paste(x$truth, collapse="")),
                    label=paste("network survival estimate on toy network ",
                                x$nrid[1]))
      })


####################################
## nsum.estimator - numerators and denominators
context("estimators - nsum numerators and denominators")

## Use the first toy network and check that tot.connections and sum.d.hat
## match hand-computed values (estimate = tot.connections / sum.d.hat)

test_that("nsum.estimator returns consistent numerator and denominator", {
  tn1 <- toy.networks[[1]]
  res <- nsum.estimator(tn1,
                        d.hat.vals      = "d",
                        y.vals          = "y",
                        total.popn.size = NA)

  ## estimate must equal tot.connections / sum.d.hat
  expect_equal(res$estimate,
               res$tot.connections / res$sum.d.hat,
               tolerance = 1e-10)

  ## components must be non-negative
  expect_true(res$tot.connections >= 0)
  expect_true(res$sum.d.hat > 0)
})

test_that("nsum.estimator uniform weights gives same result as unweighted", {
  tn1 <- toy.networks[[1]]
  tn1$uniform.weight <- 1
  res.unweighted <- nsum.estimator(tn1,
                                   d.hat.vals      = "d",
                                   y.vals          = "y",
                                   total.popn.size = NA)
  res.weighted <- nsum.estimator(tn1,
                                 d.hat.vals      = "d",
                                 y.vals          = "y",
                                 weights         = "uniform.weight",
                                 total.popn.size = NA)
  expect_equal(res.unweighted$estimate, res.weighted$estimate,
               tolerance = 1e-10)
})


####################################
## nsum.estimator - d.tot.hat pre-computed
context("estimators - nsum with pre-computed total degree")

test_that("nsum.estimator with d.tot.hat matches estimate from d.hat.vals", {
  tn1 <- toy.networks[[1]]
  ## compute total degree the normal way first
  res.normal <- nsum.estimator(tn1,
                               d.hat.vals      = "d",
                               y.vals          = "y",
                               total.popn.size = NA)

  d.tot <- res.normal$sum.d.hat

  ## now pass that total in directly
  res.dtot <- nsum.estimator(tn1,
                             d.tot.hat       = d.tot,
                             y.vals          = "y",
                             total.popn.size = NA)

  expect_equal(res.normal$estimate, res.dtot$estimate, tolerance = 1e-10)
})


####################################
## nsum.estimator - complete.obs with NAs
context("estimators - nsum complete.obs NA handling")

test_that("nsum.estimator complete.obs excludes NA rows", {
  ## Create a small synthetic dataset where we can hand-compute the answer
  ## 4 respondents: degrees c(10, 20, 30, 40), y c(1, 2, NA, 4)
  ## complete.obs uses rows 1, 2, 4 only:
  ##   tot.connections = 1 + 2 + 4 = 7
  ##   sum.d.hat       = 10 + 20 + 40 = 70
  ##   estimate (proportion) = 7/70 = 0.1
  df.na <- data.frame(d = c(10, 20, 30, 40),
                      y = c(1,   2, NA,  4))

  res <- nsum.estimator(df.na,
                        d.hat.col = "d",
                        y.col     = "y",
                        missing   = "complete.obs",
                        total.popn.size = NA)

  expect_equal(res$estimate, 0.1, tolerance = 1e-10)
  expect_equal(res$tot.connections, 7, tolerance = 1e-10)
  expect_equal(res$sum.d.hat, 70, tolerance = 1e-10)
})


####################################
## nsum.internal.consistency
context("estimators - nsum internal consistency")

test_that("nsum.internal.consistency returns expected structure and sane values", {
  ## Use the example survey data bundled with the package
  kp.q <- paste(example.knownpop.dat$known.popn)
  tot.pop.size <- 10718378
  knownpop.tot <- sum(example.knownpop.dat$size)

  survey.dat <- add.kp(example.survey,
                       df.to.kpvec(example.knownpop.dat,
                                   kp.var   = "known.popn",
                                   kp.value = "size"),
                       total.popn.size = tot.pop.size)

  survey.dat$d.hat <- kp.individual.estimator_(survey.dat,
                                               known.populations = kp.q,
                                               total.kp.size     = knownpop.tot,
                                               alter.popn.size   = tot.pop.size)$dbar.Fcell.F

  ic <- nsum.internal.consistency(survey.data     = survey.dat,
                                  missing         = "complete.obs",
                                  kp.method       = TRUE,
                                  weights         = "indweight",
                                  total.popn.size = tot.pop.size,
                                  alter.popn.size = tot.pop.size,
                                  return.plot     = FALSE,
                                  verbose         = FALSE)

  ## return list has expected keys
  expect_true(all(c("results", "mse", "rmse", "are", "mae") %in% names(ic)))

  ## all hold-out estimates are positive
  expect_true(all(ic$results$nsum.holdout.est > 0))

  ## error metrics are finite
  expect_true(is.finite(ic$mse))
  expect_true(is.finite(ic$rmse))
  expect_true(is.finite(ic$are))
  expect_true(is.finite(ic$mae))

  ## average relative error is in a plausible range for real survey data
  expect_true(ic$are < 2)
})

