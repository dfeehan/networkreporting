#####################################################
## known_population.r
##
## the known population method for estimating respondents'
## degrees
##

#####################################################
##' Average personal network size estimates using known population method
##'
##' If given \code{attribute.names}, then this function produces
##' estimated average network sizes given by the groups that are defined by 
##' all combinations of the attributes; otherwise, it estimates the
##' average personal network size for the entire frame population.
##' 
##' @section Technical note:
##' The estimated average degree is 
##' \eqn{(\sum y_{F_\alpha, A} / N_A) \times
##'      N_F / N_{F_\alpha}}
##' here, we estimate \eqn{N_F / N_{F_\alpha}} by dividing the
##' total of all respondents' weights by the sum of
##' the weights for respondents in each cell \eqn{\alpha}.
##'
##' @section TODO:
##' \itemize{
##' \item{ make unit tests }
##' \item{ think about how to elegantly add options for dbar_(P,Q) vs dbar_(Q,P)}
##' }
##'
##' @param resp.data the dataframe that has the survey responses
##' @param known.populations  the names of the columns in \code{resp.data}
##'          that have respondents' reports about connections to known populations
##' @param weights the name of a column that has sampling weights
##' @param boot.weights Optional dataframe with bootstrap resampled weights. See Details for more info.
##' @param ego.id If boot.weights are included, then this is the name of the
##'   column(s) we need to join the bootstrap weights onto the dataset. This is
##'   most often the id of the ego making the reports.
##' @param return.boot if TRUE and boot.weights are included, then return the full bootstrapped estiamates and not just the summaries;
##' this option causes this function to return a list instead of a tibble
##' @param attribute.names the names of the columns in \code{resp.data} that
##'                        determine the subgroups for which average degree is estimated;
##'                        if NULL, then the average over all respondents is estimated
##' @param total.kp.size the size of the probe alters; i.e., the sum of the known population
##'                      sizes. if NULL, then this is set to 1
##' @param alter.popn.size the size of the population of alters; this is most
##'        often the frame population, which is the default if nothing else is
##'        specified; the size of the frame population is taken to be the sum
##'        of the weights over all of resp.data
##' @param missing if "ignore", then proceed with the analysis without
##'                doing anything about missing values.
##'                if "complete.obs", then, for each row, use only the known populations
##'                that have no missingness for the
##'                computations. care must be taken in using this second option.
##'                future versions may have other options
##' @param verbose if TRUE, print information to screen
##' @return the estimated average degree (\code{dbar.Fcell.F}) for respondents in each
##'         of the categories given by \code{attribute.names}
##'
##' @section Details:
##' If you want estimated sampling variances, you can pass in a data frame \code{boot.weights}.
##' \code{boot.weights} is assumed to have a column that is named whatever the \code{ego.id} is,
##' and then a series of columns named \code{boot_weight_1}, ..., \code{boot_weight_M}.
##'
##' @rdname kp.estimator
##' @export
kp.estimator_ <- function(resp.data, 
                          known.populations,
                          weights,
                          boot.weights=NULL,
                          ego.id=NULL,
                          return.boot=FALSE,
                          attribute.names=NULL,
                          total.kp.size=NULL,
                          alter.popn.size=NULL,
                          missing="ignore",
                          verbose=TRUE) {

  wdat <- select_(resp.data, .dots=weights)
  kpdat <- select_(resp.data, .dots=known.populations)
  
  if(! is.null(ego.id)) {
    iddat <- select_(resp.data, .dots=ego.id)
  }
  
  if(! is.null(attribute.names)) {
    adat <- select_(resp.data, .dots=attribute.names)
    atnames <- paste(colnames(adat))
  } else {
    adat <- NULL
    atnames <- NULL
  }
  
  if(missing == "ignore") {
    surveybootstrap:::vcat(verbose,
                           "NOTE: Ignoring any rows with missingness on any of the report variables.\n")
  }

  ## if missing == 'complete.obs'
  ##    then disregard rows that don't have no missingness on kp questions
  
  alter.popn.size <- ifelse(is.null(alter.popn.size) ||
                            is.null(lazy_eval(alter.popn.size)),
                            sum(wdat[,1]),
                            lazy_eval(alter.popn.size))

  # NB: this will change to handle complete.obs
  total.kp.size <- ifelse(is.null(total.kp.size),
                          1,
                          lazy_eval(total.kp.size))

  # get individual-level sums for known population connections
  # (NB: might want to eventually allow for this to be adjusted by
  #      some kind of response / reporting model?)
  if(missing == 'ignore') {
    kptot <- data_frame(kptot=rowSums(kpdat, na.rm=TRUE))
  } else {
    kptot <- data_frame(kptot=rowSums(kpdat))
  }

  df <- bind_cols(kptot, wdat, adat)
  
  if (! is.null(ego.id)) {
    df <- bind_cols(df, iddat)
  }


  # now aggregate by attributes
  agg <- report.aggregator_(resp.data=df,
                            attribute.names=atnames,
                            qoi='kptot',
                            weights=weights,
                            qoi.name='y.kp')

  tograb <- lapply(c(colnames(adat),
                     'sum.y.kp', 'wgt.total.y.kp', 'num.obs.y.kp'),
                   as.symbol)
  
  ## to placate R CMD CHECK
  sum.y.kp <- NULL
  sum.y.kp.over.kptot <- NULL
  wgt.total.y.kp <- NULL
  
  res <- select_(agg, .dots=tograb) %>%
         dplyr::mutate(sum.y.kp.over.kptot = sum.y.kp / total.kp.size,
                ## here, we estimate N_F / N_{F_\alpha} by dividing the
                ## total of all respondents' weights by the sum of
                ## the weights for respondents in each cell \alpha
                ## (see technical note in documentation)
                dbar.Fcell.F = sum.y.kp.over.kptot * 
                               (alter.popn.size / wgt.total.y.kp))
    
  if (! is.null(boot.weights)) {
    
    if (is.null(ego.id)) {
      stop("In order to use bootstrap weights, you must also pass ego.id; this is needed to link the weights to the reports.")
    }
    
    df <- df %>% left_join(boot.weights,
                           by=ego.id) 
    
    
    boot.cols <- stringr::str_subset(colnames(boot.weights), ego.id, negate=TRUE)
    
    # now aggregate by attributes
    agg.boot <- report.aggregator_(resp.data=df,
                                   attribute.names=atnames,
                                   qoi='kptot',
                                   weights=boot.cols,
                                   qoi.name='y.kp')    
    
    res.boot <- select_(agg.boot, .dots=c(tograb, 'boot_idx')) %>%
      dplyr::mutate(sum.y.kp.over.kptot = sum.y.kp / total.kp.size,
                    ## here, we estimate N_F / N_{F_\alpha} by dividing the
                    ## total of all respondents' weights by the sum of
                    ## the weights for respondents in each cell \alpha
                    ## (see technical note in documentation)
                    dbar.Fcell.F = sum.y.kp.over.kptot * 
                      (alter.popn.size / wgt.total.y.kp))
      
    ## if there is any missingness in the dbar.Fcell.F estimates,
    ## warn the user (and ignore it)
    if (any(is.na(res.boot$dbar.Fcell.F))) {
      n.na <- sum(is.na(res.boot$dbar.Fcell.F))
      n.all <- length(res.boot$dbar.Fcell.F)
      warning(glue::glue("Bootstrapped degree estimates have {n.na} out of {n.all} values missing. These have been removed in the summary statistics. Beware!\n"))
    } 
    
    # get estimated sampling uncertainty for the
    # aggregated reports
    agg.varest <- res.boot %>%
      ungroup() %>%
      group_by_at(atnames) %>%
      summarise(dbar.Fcell.F.ci.low  = quantile(dbar.Fcell.F, .025, na.rm=TRUE),
                dbar.Fcell.F.ci.high = quantile(dbar.Fcell.F, 0.975, na.rm=TRUE),
                dbar.Fcell.F.median  = quantile(dbar.Fcell.F, 0.5, na.rm=TRUE),
                dbar.Fcell.F.se      = sd(dbar.Fcell.F, na.rm=TRUE))
    
    if (! is.null(atnames)) {
      # and join the estimated sampling uncertainty onto the returned asdrs
      res <- res %>%
        left_join(agg.varest, by=atnames)
    } else {
      res <- bind_cols(res, agg.varest)
    }
  }
  
  if (! is.null(boot.weights)) {
    if(return.boot) {
      res <- list(estimates = res,
                  boot.estimates = res.boot)
    }
  }

  return(res)

}

#####################################################
##' @rdname kp.estimator
##' @export
kp.estimator <- function(resp.data, 
                         known.populations,
                         weights,
                         attribute.names=NULL,
                         total.kp.size=1,
                         alter.popn.size=NULL,
                         missing='ignore') {
  
    if(is.null(attribute.names)) {
      anames <- NULL
    } else {
      anames <- lazy(attribute.names, resp.data)
    }

    kp.estimator_(resp.data,
                  known.populations=lazy(known.populations),
                  attribute.names=anames,
                  weights=lazy(weights, resp.data),
                  total.kp.size=lazy(total.kp.size),
                  alter.popn.size=lazy(alter.popn.size),
                  missing=lazy(missing))

}



#####################################################
##' Individual personal network size estimates using the known population method
##'
##' In most situations, the known population method will be
##' used to estimate the average personal network size;
##' this can be done with \code{\link{kp.estimator_}}. If, instead, you wish
##' to estimate the personal network size of each individual
##' respondent, then you can use this function.
##'
##' Note that this is not making inference about any larger population;
##' it estimates a property of each individual respondent. So the
##' sampling weights are not used here.
##'
##' @section TODO:
##' \itemize{
##' \item{ make unit tests }
##' }
##'
##' @param resp.data the respondent (survey) data
##' @param known.populations the names of the known populations
##' @param total.kp.size the sum of the sizes of all of the known populations
##' @param alter.popn.size the size of the population respondents
##'        are reporting about connections to; typically this will
##'        be the frame population, so \code{alter.popn.size} should
##'        be the size of the frame population, N.F
##' @param missing see the missing argument of \code{\link{kp.estimator_}}
##' @return a data frame with an estimate of each individual respondent's personal
##'         network size
##' @rdname kp.individual.estimator
##' @export
kp.individual.estimator <- function(resp.data, 
                                    known.populations,
                                    total.kp.size=1,
                                    alter.popn.size,
                                    missing='ignore') {

    resp.data$.rowid <- 1:nrow(resp.data)
    resp.data$.noweight <- 1

    res <- kp.estimator_(resp.data,
                         known.populations=lazy(known.populations),
                         attribute.names=".rowid",
                         weights=".noweight",
                         total.kp.size=lazy(total.kp.size),
                         alter.popn.size=lazy(alter.popn.size),
                         missing=lazy(missing))

}

#####################################################
##' @rdname kp.individual.estimator
##' @export 
kp.individual.estimator_ <- function(resp.data, 
                                     known.populations,
                                     total.kp.size=1,
                                     alter.popn.size,
                                     missing='ignore') {

    resp.data$.rowid <- 1:nrow(resp.data)
    resp.data$.noweight <- 1

    res <- kp.estimator_(resp.data,
                         known.populations=known.populations,
                         attribute.names=".rowid",
                         weights=".noweight",
                         total.kp.size=total.kp.size,
                         alter.popn.size=alter.popn.size,
                         missing=missing)

}


#####################################################
##' kp.degree.estimator (DEPRECATED)
##'
##' see \code{\link{kp.individual.estimator}} instead.
##'
##' compute an estimate of the respondents' degrees using
##' the known population method\cr
##'
##' note that this function does not take survey weights, since
##' these estimates are not for total degree, but just for the
##' individual degree of each respondent
##'
##' @param survey.data the dataframe with the survey results
##' @param known.popns if not NULL, a vector whose entries are the size of the known
##'                    populations, and whose names are the variable names in the dataset
##'                    corresponding to each one. if NULL, then assume that the survey.data
##'                    dataframe has an attribute called 'known.popns' containing this vector.
##' @param total.popn.size the size of the entire population. if NULL,
##'                        this function returns proportions; if not NULL, it
##'                        returns absolute numbers (ie, the proportions * total popn size)
##' @param missing if "ignore", then proceed with the analysis without
##'                doing anything about missing values. if "complete.obs"
##'                then, for each row, use only the known populations
##'                that have no missingness for the
##'                computations. care must be taken in using this second option
##' @param verbose if TRUE, print messages to the screen
##' @return a vector with an estimate of the degree for each row
##'         in survey.data. if missing=="ignore", then the degree for rows that have
##'         missingness in the 'how many X' questions will be set
##'         to NA
##' @export
kp.degree.estimator <- function(survey.data,
                                known.popns=NULL,
                                total.popn.size=NULL,
                                missing="ignore",
                                verbose=FALSE)
{

  ## TODO - PLAN: this should be a special wrapper around the
  ## kp.estimator_ function which produces estimates for the
  ## individual-level degree
  warning("kp.degree.estimator will be deprecated soon!")

  #### TODO
  #### this is returning a matrix now (see, eg, the rwanda mortality analysis)
  #### it should return a vector

  if (! missing %in% c("ignore", "complete.obs")) {
    stop("error in specifying procedure for handling missing values in kp.degree.estimator. invalid option.\n")
  }


  if (is.null(known.popns)) {
    known.popns <- attr(survey.data, "known.popns")
  }

  total.popn.size <- parse.total.popn.size(total.popn.size,
                                           survey.data,
                                           verbose=verbose)

  if (missing == "complete.obs") {

    ## use the modified estimator: for each row, use the known
    ## population estimator, taking populations whose responses we
    ## don't have (ie are missing) out of the numerator and
    ## denominator

    kp.dat <- survey.data[, names(known.popns)]

    ## mask for missing values: this matrix has the same shape
    ## as kp.dat, but its entries are 1 if the entry in kp.dat is
    ## observed and 0 if missing
    miss.mask <- data.matrix(as.data.frame(llply(kp.dat,
                                                 function(x) {
                                                   as.numeric(!is.na(x))
                                                 })))

    ## use the missing mask to get the sum of the N_k's for each
    ## indiviudal respondent
    ind.overall.known <- miss.mask %*% known.popns

    ind.tot.known <- (rowSums(kp.dat, na.rm=TRUE))

    res <- ind.tot.known/ind.overall.known

  } else {

    tot.known <- (rowSums(subset(survey.data,
                                 select=names(known.popns))))

    overall.known <- sum(known.popns)

    res <- tot.known/overall.known

  }

  if (! is.na(total.popn.size)) {
    res <- res * total.popn.size
  }

  return(res)

}



