#####################################################
## network_survival.r
##
## the network survival method for estimating the
## size and characteristics of a hidden population
##

#####################################################
##' network survival estimator
##'
##' use an aggregate multiplicity estimator
##' and the respondents' own network size estimates
##' to estimate hidden population sizes 
##'
##' This function takes two sources of data as input: first, it requires
##' a long-form dataframe with the attributes of the reported members of
##' the hidden population. For example, if we are asking about emigres and
##' we collect the age and sex of each reported emigrant, then the long form
##' dataset might look like:
##' \tabular{ccc}{
##'   age \tab sex \tab weight\cr
##'    15 \tab m   \tab 2.10  \cr
##'    58 \tab f   \tab 1.15  \cr
##'    33 \tab m   \tab 3.67  \cr
##' }
##'  
##'  The second source of data we need is the known population responses for the
##'  respondents, along with the *same* attributes for each respondent. For
##'  example, in the situation above, we would also require a dataset like this
##'  to be passed in
##'  \tabular{cccccc}{
##'   age  \tab sex  \tab  weight \tab hm.teachers \tab  hm.nurses  \tab ...  \cr
##'   20   \tab   f  \tab  2.10   \tab 4           \tab  0          \tab ...  \cr
##'   44   \tab   m  \tab  1.65   \tab 0           \tab  2          \tab ...  \cr
##'   60   \tab   m  \tab  2.75   \tab 1           \tab  1          \tab ...  \cr
##' }
##'
##' @section Technical note:
##' This function assumes that the sampling weights are standard analysis weights
##' and *not* relative weights. Standard analysis weights should provide an estimate
##' for the size of the frame population when added up; relative weights, on the other hand,
##' will sum to the number of respondents in the sample.  Demographic and Health surveys
##' typically have relative weights, which must be converted into standard sampling weights
##' before using this function.
##'
##' @section Returned dataframe:
##' Currently, the dataframe that is returned has columns for the attributes that death rates
##' were calculated for, along with:
##' \itemize{
##' \item{ \code{num.obs.deaths} - number of deaths reported}
##' \item{ \code{num.obs.degree} - number of respondents in cell used for estimating degree}
##' \item{ \code{y.F.Dcell.hat} - estimated total number connections from the frame to deaths}
##' \item{ \code{y.Fcell.kp.hat} - estimated total number of connections from (frame intersect cell) to the groups of known size}
##' \item{ \code{total.kp.size} - the total size of the groups of known size}
##' \item{ \code{N.Fcell.hat} - estimated size of (frame intersect cell) population (based on survey weights)}
##' \item{ \code{N.F.hat} - estimated size of frame population (based on survey weights)}
##' \item{ \code{asdr.hat} - the estimated death rate}
##' }
##'
##' @section TODO:
##' \itemize{
##' \item{ handle missing values}
##' \item{ allow passing in N.F (currently, we always estimate from weights)}
##' \item{ write more general agg mult est fn and call that }
##' \item{ make unit tests }
##' }
##' 
##' @param resp.data The dataframe that has a row for each respondent, with reported
##'                  connections to the groups of known size, as well as the attributes.
##'                  Note that the column names of the attributes should match
##'                  their names in \code{attribute.data}
##' @param attribute.data A dataframe with the reported attributes of hidden population 
##'                       members reported by survey respondents. There should be one 
##'                       row for each time a respondent reports a hidden population member.
##'                       For example, to estimate death rates, there should be one row for
##'                       each report of a death.
##' @param attribute.names The names of the columns of attribute.data
##'                        and resp.data that contain the attribute information.
##' @param known.populations The names of the columns in \code{resp.data} that
##'          have responses to the known population questions
##' @param total.kp.size The size of the probe alters, i.e., the sum of the known
##'         population sizes
##' @param weights The weights or weights column for the respondent data
##' @param attribute.weights The weights or weights column for the alter data (this should typically 
##'                          be the weight of the respondent who reported the alter)
##' @param within.alter.weights The weight or weights column for within-alter weights. This could be useful if,
##'                            for example, respondents only report about a subset of all of their alters. See Details.
##' @param boot.weights Optional dataframe with bootstrap resampled weights. See Details for more info.
##' @param ego.id If boot.weights are included, then this is the name of the
##'   column(s) we need to join the bootstrap weights onto the dataset. This is
##'   most often the id of the ego making the reports.
##' @param return.boot If TRUE, and if \code{boot.weights} is specified, then return each bootstrap estimate 
##' @param dropmiss How to handle missingness in reported connections to known populations and number of deaths. See \code{\link{report.aggregator}}
##' @param verbose If TRUE, print information to screen
##' @return the network reporting estimate of the hidden population's size
##'         (as a prevalence) broken down by the categories defined by all combinations
##'         of \code{attribute.names}.
##' @section Details:
##' If you want estimated sampling variances, you can pass in a data frame \code{boot.weights}.
##' \code{boot.weights} is assumed to have a column that is named whatever the \code{ego.id} is,
##' and then a series of columns named \code{boot_weight_1}, ..., \code{boot_weight_M}.
##' 
##' \code{ego.id} can either be a string or vector of strings, or it can be a named vector
##' like \code{c('a'='b')}. In the second case, \code{'a'} should be the name of the id column
##' in \code{resp.data}, while \code{'b'} should be the name of the id column in \code{'attribute.data'}.
##' 
##' \code{within.alter.weight} is a weight for alters within a respondent. This is set to NULL by default, and
##' many applications will not need it. However, it can be helpful if respondents
##' are only asked to report about a subset of their alters. For example, suppose that respondents can report
##' about 10 alters in detail. Respondent A reports connections to 3 deaths, and respondent B reports connections to
##' 12 deaths. When aggregating Respondent A's detailed reports, we would only need to use the sampling weight.
##' However, if we only used the sampling weight when aggregating Respondent B's reports, that would imply that
##' Respondent B only reported connections to 10 deaths. In this case, setting within.alter.weight to (12/10) for
##' Respondent B will make B's reports imply that she reported 12 deaths. Note that this makes the assumption that
##' the 10 deaths Respondent B did report are a uniformly random subsample of the 12 deaths she reports being connected to.
##'
##' @rdname network.survival.estimator
##' @export
network.survival.estimator_ <- function(resp.data,
                                        attribute.data,
                                        attribute.names,
                                        known.populations,
                                        total.kp.size=1,
                                        weights,
                                        attribute.weights,
                                        within.alter.weights=NULL,
                                        boot.weights=NULL,
                                        ego.id=NULL,
                                        return.boot=FALSE,
                                        dropmiss=FALSE,
                                        verbose=TRUE) {

    ## estimate the average personal network size of the respondents
    ## for each combination of attributes
    deg.by.att <- kp.estimator_(resp.data=resp.data,
                                known.populations=known.populations,
                                attribute.names=attribute.names,
                                weights=weights,
                                total.kp.size=total.kp.size,
                                verbose=verbose,
                                dropmiss=dropmiss)
    
    ## if within.alter.weight was specified, we use it as the scaling.factor
    ## in report.aggregator_ (see the helpfile for that function)
    if (! is.null(within.alter.weights)) {
        scaling.factor <- within.alter.weights 
    } else {
        scaling.factor <- 1
    }

    ## count the number of connections from respondents to members
    ## of the hidden population with each combination of attributes
    attribute.data$death <- 1
    deaths.by.att <- report.aggregator_(attribute.data,
                                        attribute.names,
                                        "death",
                                        attribute.weights,
                                        qoi.name="deaths",
                                        scaling.factor = scaling.factor,
                                        dropmiss=dropmiss)

    tog.df <- deg.by.att %>% full_join(deaths.by.att, by=attribute.names)

    ## the full_join could produce missing values when there is no match;
    ## this means that there is a cell that has a death and no respondents,
    ## or vice versa
    if (any(is.na(tog.df$wgt.total.y.kp))) {
        surveybootstrap:::vcat(verbose,
                               "Some deaths seem to come from a cell that has no survey respondents (or vice-versa).\n")
    }

    ## NB: we're using the sum of the sampling weights as N.F
    N.F <- sum(tog.df$wgt.total.y.kp, na.rm=TRUE)

    surveybootstrap:::vcat(verbose,
         "Taking N.F value implied by weights: ", N.F, "\n")

    ## to placate R CMD CHECK
    sum.deaths <- NULL
    sum.y.kp.over.kptot <- NULL

    ## for each combination of attributes, divide the number of
    ## reported connections by the estimated network size to produce
    ## an estimate of the overall size. then, divide this by the
    ## group size to obtain a rate.
    ## (these are done implicitly here because some of the factors cancel --
    ##  see TODO vignette)
    res <- tog.df %>%
              dplyr::mutate(total.kp.size = total.kp.size,
                            N.F.hat = N.F,
                            N.Fcell.hat = wgt.total.y.kp,
                            y.F.Dcell.hat = sum.deaths,
                            y.Fcell.kp.hat = sum.y.kp.over.kptot * total.kp.size) %>%
              dplyr::select(attribute.names,
                            # number of rows used in death reports
                            n.obs.deaths = num.obs.deaths,
                            # number of rows used in kp reports
                            n.obs.degree = num.obs.y.kp,
                            # sum of weights of rows used in death repots
                            wgt.total.deaths,
                            # sum of weights of rows used in kp reports
                            wgt.total.degree = wgt.total.y.kp,
                            y.F.Dcell.hat,
                            y.Fcell.kp.hat,
                            total.kp.size,
                            N.Fcell.hat,
                            N.F.hat) %>%
              # it may be that there are no reported connections to deaths in a given cell; the
              # estimated number of connections to deaths should then be 0
              dplyr::mutate(y.F.Dcell.hat = ifelse(wgt.total.deaths == 0, 0, y.F.Dcell.hat)) %>%
              # similarly, it may be that there are no reported connections to kp in a given cell; the
              # estimated degree should then be 0
              dplyr::mutate(y.Fcell.kp.hat = ifelse(wgt.total.degree == 0, 0, y.Fcell.kp.hat)) %>%
              dplyr::mutate(asdr.hat = (y.F.Dcell.hat / y.Fcell.kp.hat) * (total.kp.size / N.F.hat))

    ## if we've been passed bootstrap weights, use them
    if (! is.null(boot.weights)) {
        if (is.null(ego.id)) {
            stop("No ego.id specified. When using boot.weights, you must also pass ego.id in too.")
        }
        
        ## ego.id is either (1) a string or unnamed vector, in case we are good
        ##               or (2) a named vector like c('id'='ego.id'); in this case,
        ##                      the ego id has the name 'id' in the respondent data
        ##                      and 'ego.id' in the attribute.data. 
        ##                      In this situation, we'll want a character
        
        # ego.id.forjoin has the full expression for joining ego and attribute data together
        ego.id.forjoin <- ego.id  
        if (! is.null(names(ego.id))) {
            # ego.id has just the name of the variable(s) that are the ego id in the ego data
            ego.id.forjoin <- setNames(names(ego.id), ego.id)
            ego.id <- names(ego.id)
        }
        
        ## estimate the average personal network size of the respondents
        ## for each combination of attributes
        deg.by.att.boot <- kp.estimator_(resp.data=resp.data,
                                         known.populations=known.populations,
                                         attribute.names=attribute.names,
                                         weights=weights,
                                         boot.weights=boot.weights,
                                         ego.id=ego.id,
                                         ## we want all of the bootstrap resamples
                                         return.boot=TRUE,
                                         total.kp.size=total.kp.size,
                                         verbose=verbose,
                                         dropmiss=dropmiss)$boot.estimates
        
        # join bootstrap weights onto data, which is what report.aggregator_ requires
        ad.withboot <- attribute.data %>%
            left_join(boot.weights, by=ego.id.forjoin)
        
        # get the names of the columns that have bootstrap weights
        boot.cols <- stringr::str_subset(colnames(boot.weights), ego.id, negate=TRUE)
        
        ## count the number of connections from respondents to members
        ## of the hidden population with each combination of attributes
        attribute.data$death <- 1 # done above, but repeat to avoid fragility if code is changed later
        deaths.by.att.boot <- report.aggregator_(ad.withboot,
                                                 attribute.names,
                                                 qoi="death",
                                                 #attribute.weights,
                                                 weights=boot.cols,
                                                 qoi.name="deaths",
                                                 scaling.factor = scaling.factor,
                                                 dropmiss=dropmiss)
        
        tog.df.boot <- deg.by.att.boot %>% full_join(deaths.by.att.boot, 
                                                     by=c(attribute.names, 'boot_idx'))        
        
        tog.df.boot <- tog.df.boot %>%
            dplyr::mutate(total.kp.size = total.kp.size,
                          N.F.hat = N.F,
                          N.Fcell.hat = wgt.total.y.kp,
                          y.F.Dcell.hat = sum.deaths,
                          y.Fcell.kp.hat = sum.y.kp.over.kptot * total.kp.size) %>%
            dplyr::select(attribute.names,
                          # number of rows used in death reports
                          n.obs.deaths = num.obs.deaths,
                          # number of rows used in kp reports
                          n.obs.degree = num.obs.y.kp,
                          # sum of weights of rows used in death repots
                          wgt.total.deaths,
                          # sum of weights of rows used in kp reports
                          wgt.total.degree = wgt.total.y.kp,
                          y.F.Dcell.hat,
                          y.Fcell.kp.hat,
                          total.kp.size,
                          N.Fcell.hat,
                          N.F.hat,
                          boot_idx) %>%
            # it may be that there are no reported connections to deaths in a given cell; the
            # estimated number of connections to deaths should then be 0
            dplyr::mutate(y.F.Dcell.hat = ifelse(wgt.total.deaths == 0, 0, y.F.Dcell.hat)) %>%
            # similarly, it may be that there are no reported connections to kp in a given cell; the
            # estimated degree should then be 0
            dplyr::mutate(y.Fcell.kp.hat = ifelse(wgt.total.degree == 0, 0, y.Fcell.kp.hat)) %>%
            dplyr::mutate(asdr.hat = (y.F.Dcell.hat / y.Fcell.kp.hat) * (total.kp.size / N.F.hat))
        
        ## check for missingness and warn
        if (any(is.na(tog.df.boot$asdr.hat))) {
            n.na <- sum(is.na(tog.df.boot$asdr.hat))
            n.all <- length(tog.df.boot$asdr.hat)
            warning(glue::glue("Aggregate estimates have {n.na} out of {n.all} values missing. These have been removed in the summary statistics. Beware!\n"))
        } 
        
        ## calculate summaries
        boot.varest <- tog.df.boot %>%
            ungroup() %>%
            group_by_at(attribute.names) %>%
            summarise(asdr.hat.ci.low = quantile(asdr.hat, .025, na.rm=TRUE),
                      asdr.hat.ci.high = quantile(asdr.hat, 0.975, na.rm=TRUE),
                      asdr.hat.median = quantile(asdr.hat, 0.5, na.rm=TRUE),
                      asdr.hat.se = sd(asdr.hat, na.rm=TRUE))
        
        # and join the estimated sampling uncertainty onto the returned asdrs
        res <- res %>%
            left_join(boot.varest, by=attribute.names) %>%
            mutate(boot_reps = length(boot.cols))
        
        ## optionally return full bootstrap estimates
        if (return.boot) {
            res <- list(estimates=res,
                        boot.estimates=tog.df.boot)
        }
    }
    
    return(res)

}

#####################################################
##' @rdname network.survival.estimator
network.survival.estimator <- function(resp.data,
                                       attribute.data,
                                       attribute.names,
                                       known.populations,
                                       total.kp.size=1,
                                       weights,
                                       attribute.weights,
                                       within.alter.weights=NULL,
                                       boot.weights=NULL,
                                       ego.id=NULL,
                                       return.boot=FALSE,                                       
                                       dropmiss=NULL,
                                       verbose=TRUE) {

    network.survival.estimator_(resp.data,
                                attribute.data,
                                lazy(attribute.names),
                                lazy(known.populations),
                                lazy(total.kp.size),
                                lazy(weights),
                                lazy(attribute.weights),
                                lazy(within.alter.weights),
                                lazy(boot.weights),
                                lazy(ego.id),
                                lazy(return.boot),
                                lazy(dropmiss),
                                lazy(verbose))

}

