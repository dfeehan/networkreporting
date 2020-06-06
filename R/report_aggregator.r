#####################################################
## report_aggregator.r
##
## tools for aggregating respondent reports, taking
## account of weights
##

#####################################################
##' aggregate a reported quantity by groups
##' 
##' This function takes a quantity and aggregates it by groups,
##' using the design weights.
##'
##' @param resp.data the data 
##' @param attribute.names the names of the variables that define
##'          the groups for which the qoi should be aggregated
##' @param qoi the variable with quantity to aggregate
##' @param weights analysis weights; either the name of a column that has sampling weights
##' or a vector with the names of columns of the dataset that have bootstrap weights. Currently,
##' these weights must be named "boot_weight_1", "boot_weight_2", ...
##' @param scaling.factor a factor by which weights should be multiplied before applying them. Defaults to NULL (no scaling)
##' @param qoi.name the name of the qoi
##' @param dropmiss if TRUE, then drop missing values and rescale the weights to preserve their total. So, if weights
##' sum to 100, and dropping rows with missing values leads to weights that sum to 80, then the remaining rows will have
##' their weights multiplied by (100/80) to ensure the weights still add up to 100 after dropping the rows with missing values.
##' Defaults to FALSE
##' @param TODO TODO
##' @return the aggregated reported quantities
##' @rdname report.aggregator
report.aggregator_ <- function(resp.data,
                               attribute.names,
                               qoi,
                               weights,
                               qoi.name,
                               scaling.factor=NULL,
                               dropmiss=FALSE) {
  
  resp.data <- resp.data

  wdat <- select_(resp.data, .dots=weights)
  qdat <- select_(resp.data, .dots=qoi)
  adat <- select_(resp.data, .dots=attribute.names)
  
  ## multiply weights by scaling factor, if one was specified
  if (! is.null(scaling.factor)) {
    if (is.numeric(scaling.factor)) {
      # scaling factor is a number or vector, which we'll multiply each weight by
      wdat <- wdat %>%
        dplyr::mutate_all(~ . * scaling.factor)
    } else if (is.character(scaling.factor)) {
      if(! scaling.factor %in% colnames(resp.data)) {
        stop(glue::glue("report.aggregator_: Can't find column '{sf}', which is supposed to have a scaling factor.",
                         sf = scaling.factor))
      }
      # scaling factor is a string, meaning it's a column name
      wdat <- wdat %>%
        dplyr::mutate_all(~ . * resp.data[[scaling.factor]])
    }
  }

  df <- bind_cols(wdat, qdat, adat)

  # see
  # http://stackoverflow.com/questions/21208801/group-by-multiple-columns-in-dplyr-using-string-vector-input

  wgt.col <- as.symbol(names(df)[1])
  qoi.col <- as.symbol(names(df)[2])

  weighted_sum <- function(x, w, na.rm = FALSE) 
  { 
    if(na.rm) {
      idx <- (1:length(x))[!is.na(x)]
      wfac <- sum(w) / sum(w[idx])
    } else {
      idx <- 1:length(x)
      wfac <- 1
    }
    return(sum(x[idx]*w[idx]*wfac)) 
  }
  
  weighted_mean <- function (x, w, na.rm = FALSE) 
  {
    if (na.rm) {
      idx <- (1:length(x))[!is.na(x)]
    }
    else {
      idx <- 1:length(x)
    }
    return(sum(x[idx] * w[idx])/sum(w[idx]))
  }

  ## NB: this design is based on siblingsurvival::get_ind_est_from_ec
  df.summ <- df %>% 
             group_by_at(attribute.names) %>%
             dplyr::summarise_at(.vars=weights,
                                 .funs=list(mean.qoi = ~weighted_mean(x=.data[[qoi]], w=., na.rm=dropmiss),
                                            sum.qoi  = ~weighted_sum(x=.data[[qoi]], w=., na.rm=dropmiss),
                                            # get the sum of the weights
                                            # NB: this includes rows w/ missingness, even if
                                            #     dropmiss=TRUE
                                            wgt.total = ~sum(.),
                                            wgt.inv.total = ~sum(1/.),
                                            ## this is a hack because n() generates errors due to
                                            ## plyr/dplyr import conflict (and it is hard to regulate
                                            ## import order with package infrastructure)
                                            num.obs = ~length(.),
                                            dropmiss = ~dropmiss))
  
  ## if we have bootstrap weights, reshape and clean things up
  if(length(weights) > 1) {
    
    ## NB: assuming weight names are boot_weight_1, boot_weight_2, ...
    df.summ <- df.summ %>%
      tidyr::gather(starts_with('boot_weight'),
             key='rawqty',
             value='value')  %>%
      mutate(qty = stringr::str_remove(rawqty, 'boot_weight_\\d+_'),
             boot_idx = as.integer(stringr::str_remove_all(rawqty, '[^\\d]'))) %>%
      select(-rawqty) %>%
      tidyr::spread(qty, value)
    
  } #else {
  
      toren <- list(~mean.qoi, ~sum.qoi, ~wgt.total, ~wgt.inv.total, ~num.obs)
      newnames <- paste0(c("mean.", "sum.", "wgt.total.", 
                           "wgt.inv.total.", "num.obs."), qoi.name)
    
      df.summ <- dplyr::rename_(df.summ,
                         .dots=setNames(toren, newnames))
  #}

  return(df.summ)

}

#####################################################
##' @rdname report.aggregator
report.aggregator <- function(resp.data,
                              attribute.names=NULL, 
                              qoi,
                              weights,
                              qoi.name=NULL,
                              scaling.factor=NULL,
                              dropmiss=FALSE) {

    report.aggregator_(resp.data,
                       attribute.names=lazy(attribute.names, env=resp.data),
                       qoi=lazy(qoi, resp.data),
                       weights=lazy(weights, resp.data),
                       ifelse(is.null(qoi.name), "qoi", lazy(qoi.name)),
                       scaling.factor=lazy(scaling.factor, resp.data),
                       dropmiss)

}

