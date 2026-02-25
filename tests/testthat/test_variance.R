## TODO -- test that there is some variance across a set of bootstrap
##         samples

## TODO -- for ratios, test also that there is some variance in
##         numerator and denominator

## TODO -- test paired ego / alter datasets

## TODO -- test that calling bootstrap.estimates works when
##         total.popn.size is an argument and not an attribute of
##         the data frame (had to use parent.frame(2)) to fix
##         a bug about this

## TODO -- test cases where estimates should never be negative

## TODO -- look at
## http://stackoverflow.com/questions/8898469/is-it-possible-to-use-r-package-data-in-testthat-tests-or-run-examples
## to try and figure out the real way to include package data in
## unit tests...

set.seed(12345)

## rescaled.bootstrap.sample was updated to return tibbles with a
## 'weight_scale' column (underscore), but bootstrap.estimates still reads
## 'weight.scale' (dot), causing numeric(0) weights and a crash.
## This adapter renames the column to restore compatibility.
## It must be assigned to the global environment because bootstrap.estimates
## uses get.fn() to look up the bootstrap function by name in the package
## namespace / global env (a local function in the test file is not visible).
rescaled.bootstrap.compat <- function(survey.data, survey.design, num.reps,
                                      parallel = FALSE, paropts = NULL) {
  reps <- rescaled.bootstrap.sample(survey.data    = survey.data,
                                    survey.design  = survey.design,
                                    num.reps       = num.reps,
                                    parallel       = parallel,
                                    paropts        = paropts)
  lapply(reps, function(r) {
    names(r)[names(r) == "weight_scale"] <- "weight.scale"
    r
  })
}
assign("rescaled.bootstrap.compat", rescaled.bootstrap.compat, envir = .GlobalEnv)

## size of the entire population
tot.pop.size <- 10718378

## column names for connections to groups of known size
kp.q <- paste(example.knownpop.dat$known.popn)


boot.example <- example.survey
attr(boot.example, 'total.popn.size') <- tot.pop.size

boot.example <- add.kp(boot.example,
                       df.to.kpvec(example.knownpop.dat,
                                   kp.var='known.popn',
                                   kp.value='size'))

knownpop.tot <- sum(example.knownpop.dat$size)

boot.example$d.hat <- kp.individual.estimator_(boot.example, 
                               known.populations=kp.q,
                               total.kp.size=knownpop.tot,
                               alter.popn.size=tot.pop.size)$dbar.Fcell.F

M1 <- 10
M2 <- 50

test.bootfn <- function(bfn) {

  boot1 <- bootstrap.estimates(survey.data=boot.example,
                               survey.design= ~ cluster + strata(region),
                               num.reps=M1,
                               estimator.fn="nsum.estimator",
                               ## these args below all go to
                               ## the estimator
                               kp.method=TRUE,
                               return.plot=FALSE,
                               weights="indweight",
                               missing="complete.obs",
                               y.vals="clients",
                               verbose=FALSE,
                               bootstrap.fn=bfn,
                               d.hat.vals="d.hat")

  boot2 <- bootstrap.estimates(survey.data=boot.example,
                               survey.design= ~ cluster + strata(region),
                               num.reps=M2,
                               estimator.fn="nsum.estimator",
                               ## these args below all go to
                               ## the estimator
                               kp.method=TRUE,
                               return.plot=FALSE,
                               weights="indweight",
                               missing="complete.obs",
                               y.vals="clients",
                               verbose=FALSE,
                               bootstrap.fn=bfn,
                               d.hat.vals="d.hat")

  ests1 <- laply(boot1, function(x) { x$estimate })
  nums1 <- laply(boot1, function(x) { x$tot.connections })
  denoms1 <- laply(boot1, function(x) { x$sum.d.hat })

  ests2 <- laply(boot2, function(x) { x$estimate })
  nums2 <- laply(boot2, function(x) { x$tot.connections })
  denoms2 <- laply(boot2, function(x) { x$sum.d.hat })

  ## be sure that there is variation in the numerator
  ## the denominator and the estimates
  expect_false(var(ests1) == 0)
  expect_false(var(nums1) == 0)
  expect_false(var(denoms1) == 0)

  expect_false(var(ests2) == 0)
  expect_false(var(nums2) == 0)
  expect_false(var(denoms2) == 0)

  ## be sure that the estimates, the numerator,
  ## and the denominator are always nonnegative
  expect_true(all(ests1 >= 0))
  expect_true(all(nums1 >= 0))
  expect_true(all(denoms1 >= 0))

  expect_true(all(ests2 >= 0))
  expect_true(all(nums2 >= 0))
  expect_true(all(denoms2 >= 0))

  ## eventually, we might want to do more with the resamples,
  ## so return them
  return(list(boot1, boot2))
}

#########################################
## simple random sample (SRS) bootstrap
context("variance estimators - srs bootstrap - sanity checks")

test_that("srs bootstrap produces variance and non-negative estimates", {
  test.bootfn("srs.bootstrap.sample")
})


#########################################
## rescaled (Rao / Wu) bootstrap
context("variance estimators - rescaled bootstrap - sanity checks")

test_that("rescaled bootstrap produces variance and non-negative estimates", {
  test.bootfn("rescaled.bootstrap.compat")
})


## TODO -- LEFT OFF HERE...
##  * consider increasing M

#########################################
## bootstrap mean close to raw estimate
context("variance estimators - bootstrap mean vs raw estimate")

test_that("mean of bootstrap estimates is reasonably close to raw point estimate", {
  raw <- nsum.estimator(boot.example,
                        kp.method       = TRUE,
                        weights         = "indweight",
                        missing         = "complete.obs",
                        y.vals          = "clients",
                        verbose         = FALSE,
                        d.hat.vals      = "d.hat")

  boot.res <- bootstrap.estimates(
    survey.data    = boot.example,
    survey.design  = ~ cluster + strata(region),
    num.reps       = M2,
    estimator.fn   = "nsum.estimator",
    kp.method      = TRUE,
    return.plot    = FALSE,
    weights        = "indweight",
    missing        = "complete.obs",
    y.vals         = "clients",
    verbose        = FALSE,
    bootstrap.fn   = "rescaled.bootstrap.compat",
    d.hat.vals     = "d.hat"
  )

  boot.ests <- laply(boot.res, function(x) { x$estimate })

  ## the mean of the bootstrap estimates should be within 50% of the raw estimate
  expect_true(abs(mean(boot.ests) - raw$estimate) / raw$estimate < 0.5)
})


## raw estimates
## rw.raw <- nsum.internal.validation(rw.data,
##                                    kp.method=TRUE,
##                                    na.rm=TRUE,
##                                    weights="indweight")

## conduct the bootstrap resamples
## rw.rbs <- rw.befn(bootstrap.fn="rescaled.bootstrap.compat")
## rw.srs <- rw.befn(bootstrap.fn="srs.bootstrap.sample")
