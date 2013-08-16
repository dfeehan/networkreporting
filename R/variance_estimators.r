#####################################################
## variance_estimators.R
##
## this file has the functions that produce
## variance estimates

#####################################################
##' killworth.se
##'
##' compute standard errors for scale-up estimates
##' based on the Killworth estimator
##'
##' note that this is provided for comparison, but
##' that we do not generally recommend using this
##' strategy for estimating variance
##'
##' @param estimates TODO
##' @param d.hat TODO
##' @param total.popn.size TODO
##' @param total TODO
##' @param missing TODO
##' @return the estimated standard error
##' @keywords internal
killworth.se <- function(estimates,
                         d.hat,
                         total.popn.size=NULL,
                         total=TRUE,
                         missing="ignore") {

  stop("killworth.se is not yet implemented.")

  ## TODO -- code below this point not yet altered...

  if (total & is.null(total.popn.size)) {
    stop("you must pass in total.popn.size to get the Killworth variance estimates of the total (rather than proportions).")
  }

  na.rm <- ifelse(missing == "ignore", TRUE, FALSE)

  sum.d.hat <- sum(d.hat, na.rm=na.rm)

  est.props <- ifelse(rep(total, length(estimates)),
                      estimates / total.popn.size,
                      estimates)

  res <- aaply(est.props,
               1,
               function(est) {
                 return(sqrt((est*(1-est))/sum.d.hat))
               })

  names(res) <- names(estimates)

  if (total) {
    res <- res*total.popn.size
  }

  return(res)
}


#####################################################
##' bootstrap.estimates
##'
##' this function contains the core of the rescaled bootstrap
##' method for estimating uncertainty in our estimates
##' it should be designed so that it can be passed in to
##' estimation functions as an argument\cr
##' OR\cr
##' \cr
##' TODO -- estimator.fn/bootstrap.fn and summary.fn are treated differently
##' (one expects characters, one expects an actual fn. fix!)\cr
##' \cr
##' TODO -- write description block, including estimator.fn, bootstrap.fn,
##' summary.fn, more?
##' \cr
##'
##' @param survey.data the dataset to use
##' @param survey.design a formula describing the design of the survey
##'                      (see below - TODO)
##' @param estimator.fn name of a function which, given a dataset like
##'                     survey data and arguments in \code{...},
##'                     will produce an estimate of interest
##' @param bootstrap.fn name of the method to be used to take
##'                     bootstrap resamples; see below
##' @param num.reps the number of bootstrap replication samples to draw
##' @param weights weights to use in estimation (or NULL, if none)
##' @param summary.fn (optional) name of a function which, given the set of estimates
##'                   produced by estimator.fn, summarizes them. if not specified, all of
##'                   the estimates are returned in a list
##' @param parallel if TRUE, use the plyr library's .parallel argument to
##'                 produce bootstrap resamples and estimates in parallel
##' @param paropts if not NULL, additional arguments to pass along to the
##'                parallelization routine
##' @param verbose if TRUE, produce lots of feedback about what is going on
##' @param ... additional arguments which will be passed on to the estimator fn
##' @return if no summary.fn is specified, then return the list of estimates
##'         produced by estimator.fn; if summary.fn is specified, then return
##'         its output
##' @export
##' @examples
##' \donttest{
##' # code goes here
##' ...
##' }
bootstrap.estimates <- function(survey.data,
                                survey.design,
                                bootstrap.fn,
                                estimator.fn,
                                num.reps,
                                weights=NULL,
                                ...,
                                summary.fn=NULL,
                                verbose=TRUE,
                                parallel=FALSE,
                                paropts=NULL)
{

  ## get the weights
  weights <- get.weights(survey.data, weights)

  ## build up a single call to obtain an actual bootstrap
  ## replicate; we'll call this once for each one...
  boot.call <- match.call()

  boot.arg.idx <- match(c("survey.data",
                          "survey.design",
                          "num.reps",
                          "parallel",
                          "paropts"),
                        names(boot.call),
                        0L)
  boot.call <- boot.call[c(1,boot.arg.idx)]
  boot.call[[1]] <- get.fn(bootstrap.fn)

  ## also build up a call to obtain an estimate from the data
  est.call <- match.call(expand.dots=TRUE)

  ## these are the args we *won't* use when we call the estimator
  ## (ie, we use them here or in the bootstrap fn instead)
  est.arg.idx <- match(c("survey.design",
                         "estimator.fn",
                         "num.reps",
                         ##"weights",
                         "summary.fn",
                         "bootstrap.fn",
                         "parallel",
                         "paropts",
                         ## don't use survey.data b/c we're going to pass
                         ## in a bootstrap resample each time instead...
                         "survey.data"),
                       names(est.call),
                       0L)
  est.call <- est.call[-est.arg.idx]
  est.call[[1]] <- get.fn(estimator.fn)

  ## get the bootstrap samples
  boot.idx <- eval(boot.call, parent.frame())

  ## produce our estimate for each one
  res <- llply(boot.idx,

               function(this.rep) {

                 ## use the resampled indices to construct
                 ## a full resampled dataset
                 tmpdat <- survey.data[this.rep$index,]
                 tmpweights <- weights[this.rep$index]

                 ## apply the weight.scale to the estimation weights
                 tmpweights <- tmpweights * this.rep$weight.scale

                 ## add the information about which rows in the individual dataset
                 ## these resamples come from as an attribute
                 attr(tmpdat, "resampled.rows.orig.idx") <- this.rep$index

                 est.call[["survey.data"]] <- tmpdat
                 est.call[["weights"]] <- tmpweights

                 ## call estimator.fn to produce an estimate from
                 ## the bootstrap-resampled dataset
                 this.est <- eval(est.call, parent.frame(2))

                 return(this.est)
               },
               .parallel=parallel,
               .paropts=paropts)

  ## if the user specified a summary function, use it
  if (! is.null(summary.fn)) {

    this.sf <- get.fn(summary.fn)
    res <- do.call(this.sf, list(res))

  }

  return(res)

}

#####################################################
##' rescaled.bootstrap.sample
##'
##' given a survey dataset and a description of the survey
##' design (ie, which combination of vars determines primary sampling
##' units, and which combination of vars determines strata), take
##' a bunch of bootstrap samples for the rescaled bootstrap estimator
##' (see, eg, Rust and Rao 1996).
##'
##' Note that we assume that the formula uniquely specifies PSUs.
##' This will always be true if the PSUs were selected without replacement.
##' If they were selected with replacement, then it will be necessary
##' to make each realization of a given PSU in the sample a unique id.
##' Bottom line: the code below assumes that all observations within
##' each PSU (as identified by the design formula) are from the same draw
##' of the PSU.
##'
##' The rescaled bootstrap technique works by adjusting the
##' estimation weights based on the number of times each
##' row is included in the resamples. If a row is never selected,
##' it is still included in the returned results, but its weight
##' will be set to 0. It is therefore important to use estimators
##' that make use of the estimation weights on the resampled
##' datasets.
##'
##' We always take m_i = n_i - 1, according to the advice presented
##' in Rao and Wu (1988) and Rust and Rao (1996).
##'
##' @param survey.data the dataset to use
##' @param survey.design a formula describing the design of the survey (see below - TODO)
##' @param num.reps the number of bootstrap replication samples to draw
##' @param parallel if TRUE, use parallelization (via \code{plyr})
##' @param paropts an optional list of arguments passed on to \code{plyr} to control
##'        details of parallelization
##' @return a list with \code{num.reps} entries. each entry is a dataset which
##' has at least the variables \code{index} (the row index of the original
##' dataset that was resampled) and \code{weight.scale}
##' (the factor by which to multiply the sampling weights
##' in the original dataset).
##' @details \code{survey.design} is a formula of the form\cr
##'    weight ~ psu_vars + strata(strata_vars),
##' where weight is the variable with the survey weights and psu
##' is the variable denoting the primary sampling unit
##' @export
rescaled.bootstrap.sample <- function(survey.data,
                                      survey.design,
                                      parallel=FALSE,
                                      paropts=NULL,
                                      num.reps=1)
{

  survey.data$.internal_id <- 1:nrow(survey.data)

  design <- parse_design(survey.design)

  ## drop the "~" at the start of the formula
  psu.vars <- design$psu.formula[c(-1)][[1]]

  ## in the special case where there are no PSU vars, treat each row as
  ## its own PSU
  if (length(psu.vars)==1 & psu.vars=="1") {
    psu.vars <- as.name(".internal_id")
  }
  
  ## if no strata are specified, enclose the entire survey all in
  ## one stratum
  if (is.null(design$strata.formula)) {
    strata <- list(survey.data)
  } else {
    strata <- dlply(survey.data, design$strata.formula, identity)
  }

  ## get num.reps bootstrap resamples within each stratum,
  ## according to the rescaled bootstrap scheme
  ## (see, eg, Rust and Rao 1996)
  bs <- llply(strata,
              function(stratum.data) {

                ## figure out how many PSUs we have in our sample
                psu.count <- count(stratum.data,
                                   psu.vars)

                n.h <- nrow(psu.count)

                ## take m_h = n_h - 1, which is what the literature
                ## most commonly recommends
                m.h <- n.h - 1

                resamples <- llply(1:num.reps,
                                   function(rep) {

                                     ## sample m.h PSUs with replacement
                                     these.psu.samples <- sample(1:nrow(psu.count),
                                                                 m.h,
                                                                 replace=TRUE)

                                     ## r.hi is the number of times PSU i in stratum
                                     ## h was chosen in our resample
                                     r.hi <- count(data.frame(psu.row=these.psu.samples))

                                     r.hi$weight.scale <- r.hi$freq * (n.h / m.h)

                                     psu.count$freq <- NULL

                                     psu.count$r.hi <- 0
                                     psu.count$weight.scale <- 0

                                     psu.count[ r.hi$psu.row, "r.hi" ] <- r.hi$freq

                                     ## this is the factor by which we
                                     ## need to multiply
                                     ## the sampling weights
                                     ## (again, see, eg, Rust and Rao 1996, pg 292)
                                     psu.count[ r.hi$psu.row,
                                                "weight.scale" ] <- r.hi$weight.scale

                                     this.resample <- merge(stratum.data[,c(all.vars(psu.vars),
                                                                            ".internal_id")],
                                                            psu.count,
                                                            by=all.vars(psu.vars))
                                     this.resample$.internal_id.1 <- NULL


                                     return(this.resample)
                                   },
                                   .parallel=parallel,
                                   .paropts=paropts)

                return(resamples)
              })

  # now reassemble each stratum...
  res <- llply(1:num.reps,
               function(rep.idx) {
                 this.rep <- ldply(bs,
                                   function(this.stratum.samples) {
                                     return(this.stratum.samples[[rep.idx]])
                                   })

                 this.rep <- rename(this.rep,
                                    c(".internal_id"="index"))

                 return(this.rep)
               })

  return(res)
}

#####################################################
##' srs.bootstrap.sample
##'
##' given a survey dataset and a description of the survey
##' design (ie, which combination of vars determines primary sampling
##' units, and which combination of vars determines strata), take
##' a bunch of bootstrap samples under a simple random sampling
##' (with repetition) scheme
##'
##' @param survey.data the dataset to use
##' @param num.reps the number of bootstrap replication samples to draw
##' @param parallel if TRUE, use parallelization (via \code{plyr})
##' @param paropts an optional list of arguments passed on to \code{plyr} to control
##'        details of parallelization
##' @param ... ignored, but useful because it allows params like
##\code{survey.design},
##' which are used in other bootstrap designs, to be passed in without error
##' @return a list with \code{num.reps} entries. each entry is a dataset which has
##' at least the variables \code{index} (the row index of the original dataset that
##' was resampled) and \code{weight.scale} (the factor by which to multiply the
##' sampling weights in the original dataset).
##'
##' @details \code{survey.design} is not needed; it's included as an argument
##' to make it easier to drop \code{srs.bootstrap.sample} into the place of
##' other bootstrap functions, which do require information about the survey design
##' @export
srs.bootstrap.sample <- function(survey.data,
                                 num.reps=1,
                                 parallel=FALSE,
                                 paropts=NULL,
                                 ...)
{

  survey.data$.internal_id <- 1:nrow(survey.data)

  res <- llply(1:num.reps,
               function(rep.idx) {

                 these.samples <- sample(1:nrow(survey.data),
                                         nrow(survey.data),
                                         replace=TRUE)

                 this.rep <- data.frame(index=these.samples,
                                        weight.scale=1)

                 return(this.rep)
               },
               .parallel=parallel,
               .paropts=paropts)

  return(res)
}
