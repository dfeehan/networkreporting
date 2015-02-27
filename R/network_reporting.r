#####################################################
## network_reporting.r
##
## the network reporting method for estimating the
## size and characteristics of a hidden population
##
## TODO -- this may get renamed; network reporting is
##         really a word for a family of methods
## TODO -- handle NAs in code below (this is partially done)
## TODO -- make handling of column vars, etc, uniformly handled
##         when passed in as args. (ie, use get.var; now some fns
##         still don't use get.var)
## TODO -- perhaps an easy to use interface for nsum estimates
##         with bootstrap (and then use this in nsum.internal.validation)
## TODO -- rename 'network rate' stuff to 'network reporting'
## TODO -- when using defaults (for example, taking
##         popn size info from dataframe attributes,
##         should we print out a message to the screen?
##         or perhaps have a (default) verbose mode?)
## TODO -- think about code to get 45q15 from these data...


#####################################################
##' networkreporting.estimator
##'
##' compute the network reporting estimate of the
##' hidden population's size.
##' this function takes two sources of data as input: first, it requires
##' a long-form dataframe with the attributes of the reported members of
##' the hidden population. for example, if we are asking about emigres and
##' we collect the age and sex of each reported emigrant, then the long form
##' dataset might look like
##' \tabular{cc}{
##'   age \tab sex\cr
##'    15   \tab m\cr
##'    58   \tab f\cr
##'    33   \tab m\cr
##' }
##'  note that, in this dataset, we do not need to know which respondent
##'  reported each row. (this may change once we work out
##'  weighting, et cetera).
##'  the second source of data we need is the degree estimates for the
##'  respondents, along with the *same* attributes for each respondent. for
##'  example, in the situation above, we would also require a dataset like this
##'  to be passed in
##'  \tabular{ccc}{
##'   d.hat \tab  age \tab  sex\cr
##'    150 \tab  20 \tab   f\cr
##'    163 \tab  44 \tab   m\cr
##'    110 \tab  60 \tab   m\cr
##' }\cr
##' TODO - eventually handle weights? this is hard, i think. also be sure to
##'        use the get.weights helper function instead of the weights.col
##'        argument used below\cr
##' TODO - more housekeeping to check the two data sets agree (see comment
##'              inline, below)\cr
##' TODO - eventually handle degree ratios...\cr
##' TODO -  eventually allow specific columns to be picked out of
##'         dataframe\cr
##' TODO - eventually handle factors and continuous vars...\cr
##' TODO - for now, we're assuming that if you there is a combination of
##'       attributes that results in a total number known of NA, then that
##'       should be 0 (ie, we're assuming no alters were reported with
##'       that combination of attributes). this means that it's currently
##'       important to handle missing data *before* calling this function
##' 
##' @param resp.data the dataframe that has the estimated degree of each respondent,
##'                   along with the same characteristics reported for each alter
##'                   that are found in attribute.data. note that the estimated
##'                   degree should be in the column indicated by d.hat.col,
##'                   and that the column names of the attributes should match
##'                   their names in attribute.data
##' @param attribute.data the long-form dataframe with reported attributes of
##'                       alters in the subgp we're studying.
##' @param attribute.names if not NULL, the names of the columns of attribute.data
##'                        and resp.data that contain the attribute information.
##'                        if NULL, then all of the columns of attribute.data are
##'                        assumed to have attribute information
##' @param resp.attribute.names if not NULL, the names of the columns of the
##'                        respondent data that contain the respondent's attributes.
##'                        these should correspond to the entries in
##'                        attribute.names.  if NULL, then
##'                        the names are taken to be the same as attribute.names
##' @param d.hat.vals  the name or index of the column with the degree estimates in
##'                   resp.data; all the other columns are assumed to be attributes
##' @param weights if not NULL, weights to use in computing the estimate. this
##'                should be the name of the column in the resp.data which has
##'                the variable with the appropriate weights. these weights
##'                should be construted so that, eg, the mean of the degrees is
##'                estimated as (1/n) * \\sum_i {w_i * d_i}
##' @param attribute.weights the weights for the attribute variable. if NULL,
##'        assume these are the same as weights
##' @return the network reporting estimate of the hidden population's size
##'         (as a prevalence)
##'         broken down by the categories given
##' @export
networkreporting.estimator <- function(resp.data,
                                       attribute.data,
                                       attribute.names=NULL,
                                       resp.attribute.names=NULL,
                                       d.hat.vals=NULL,
                                       weights=NULL,
                                       attribute.weights=NULL)
 
{

  if (is.null(attribute.names)) {
    attribute.names <- colnames(attribute.data)
  }

  if (is.null(resp.attribute.names)) {
    resp.attribute.names <- attribute.names
  }

  if (is.null(attribute.weights)) {
    attribute.weights <- weights
  }

  if (length(resp.attribute.names) != length(attribute.names)) {
    stop("need names for the respondent attributes to correspond to the names of the alters' attributes.\n")
  }

  weights <- get.weights(resp.data, weights)
  
  attribute.weights <- get.weights(attribute.data,
                                   attribute.weights)
                                   
  attribute.data$.internal_att_weight <- attribute.weights
    
  ## TODO -- check that attributes are the same in the respondent data
  ##         and in the alter data

  #######################################################
  ## denominator, the distribution of degree estimates
  ## in the population.
  ## (this comes fromt the ego data)
  deg.tot <- alter.distn.estimator(resp.data=resp.data,
                                   d.hat.vals=d.hat.vals,
                                   attribute.names=resp.attribute.names,
                                   weights=weights)

  ## at this point, tot.known should have the estimated
  ## degree distribution of the set of alters

  N.F.hat <- sum(1/weights)

  #######################################################  
  ## summarize attribute distribution in the reported alters
  ## (eg, outmigrants, deaths, etc)
  ## (this comes from the alter data)
  alter.tot <- ddply(attribute.data,
                     as.quoted(attribute.names),
                     function(x) {
                       res <- sum(x$.internal_att_weight)
                       ##res <- sum(1/x$.internal_att_weight)

                       altercount <- nrow(x)

                       ## res <- sum(x[,attribute.weight.col])
                       return(data.frame(tot.known=res,
                                         altercount=altercount,
                                         mean.known=mean(x$.internal_att_weight),
                                         hajek.mean.known=res/N.F.hat))
                       #return(data.frame(tot.known=res,
                       #                  hajek.mean.known=res/N.F.hat))
                     })  

  ## TODO -- we should do more housekeeping to be sure that the
  ## alter degree distribution lines up appropriately here
  ## that is, we wish to be sure that deg.tot and alter.tot
  ## are both classified by the same factors, etc
  
  ## be sure that the factor levels are the same
  ## for each attribute, in both the respondent
  ## and alter datasets
  for(i in seq_along(attribute.names)) {

    if (length(levels(deg.tot[,attribute.names[i]])) <
        length(levels(attribute.data[,attribute.names[i]]))) {
      
          deg.tot[,resp.attribute.names[i]] <-
               harmonize.levels(deg.tot[,resp.attribute.names[i]],
                                attribute.data[,attribute.names[i]])
          
    } else {
      
      attribute.data[,attribute.names[i]] <-
        harmonize.levels(attribute.data[,attribute.names[i]],
                         deg.tot[,resp.attribute.names[i]])
      
    }

  }

  ## combine the alter and degree estimates by category
  comb <- merge(deg.tot,
                alter.tot,
                by.x=resp.attribute.names,
                by.y=attribute.names,
                all=TRUE)

  ## and produce the estimate...
  comb <- transform(comb,
                    ##estimate=tot.known/sum.degree,
                    ## NB: avg number of connections to deaths in group
                    ## is the number of alters in the group / wgt number
                    ## of *survey respondents*
                    estimate=(altercount/wgt.num.obs)/mean.degree)

  return(comb)

}

#####################################################
##' estimate the degree distribution of the multiset
##' of alters using a direct approach.
##' 
##' this function uses estimated degrees for each survey respondent
##' to estimate the degrees of the multiset of alters. it must be
##' given the degree estimates for the
##' respondents, along with the *same* attributes for each respondent. for
##' example, it requires a dataset like this
##' to be passed in\cr
##' \tabular{ccc}{
##'   d.hat   \tab age  \tab sex \cr
##'    150    \tab 20   \tab f \cr
##'    163    \tab 44   \tab m \cr
##'    110    \tab 60   \tab m \cr
##' }
##'
##' \itemize{
##' \item{TODO}{ eventually handle weights? this is hard, i think}
##' \item{TODO}{ eventually handle degree ratios...}
##' \item{TODO}{ eventually allow specific columns to be picked out of
##'       dataframe}
##' \item{TODO}{ eventually handle factors and continuous vars...}
##' \item{TODO}{ for now, we're assuming that if you there is a combination of
##'       attributes that results in a total number known of NA, then that
##'       should be 0 (ie, we're assuming no alters were reported with
##'       that combination of attributes). this means that it's currently
##'       important to handle missing data *before* calling this function}
##' }
##'
##' @param resp.data the dataframe that has the estimated degree of each respondent,
##'                   along with the same characteristics reported for each alter that
##'                   are found in attribute.data. note that the estimated degree should
##'                   be in the column indicated by d.hat.col, and that the column names
##'                   of the attributes should match their names in attribute.data
##' @param attribute.names if not NULL, the names of the columns of the respondent
##'                        data that contain the respondent's attributes. if NULL, then
##'                        the names are taken to be the non-degree columns of resp.data
##' @param d.hat.vals  the name or index of the column with the degree estimates in
##'                   resp.data; all the other columns are assumed to be attributes
##' @param dropmiss if TRUE, drop combinations of attributes that have missing
##'                 average degree estimates; if TRUE, keep them
##' @param weights if not NULL, weights to use in computing the estimate. this
##'                should be the name of the column in the resp.data which has
##'                the variable with the appropriate weights. these weights
##'                should be construted so that, eg, the mean of the degrees is
##'                estimated as (1/n) * \\sum_i {w_i * d_i}
##' @return the dansum estimate of the hidden population's size (as a prevalence)
##'         broken down by the categories given
##' @export
##' @examples
##'
##' ## example 1
##' altdist <- alter.distn.estimator(resp.data=thisdat,
##'                                  attribute.names=these.att,
##'                                  d.hat.col="d.hat",
##'                                  dropmiss=FALSE)
alter.distn.estimator <- function(resp.data,
                                  attribute.names,                                
                                  d.hat.vals="d.hat",
                                  dropmiss=FALSE,
                                  weights=NULL) {

  
  weights <- get.weights(resp.data, weights)

  ## grab degree estimates from survey dataframe
  d.vals <- get.var(resp.data, d.hat.vals)
  
  if (is.null(attribute.names)) {
    
    ## TODO -- do we still want to match names of
    ## weight.col and d.hat.vals here?
    ##attribute.names <- colnames(resp.data)[-match(colnames(resp.data,
    ##                                                       c(d.hat.vals,
    ##                                                         weight.col)))]
    ## take out the columns that have the degrees and weights,
    ## if they are in the data
    ## TODO -- I don't think this will work, as it is currently written...
    stop("this case (NULL attribute.names in alter degree estimation) doesn't work yet")
    attribute.names <- colnames(resp.data)[-match(colnames(resp.data,
                                                           c(d.hat.vals,
                                                             weight.col),
                                                           0L))]    
  }

  ## apply the weights to the degree estimates
  ##d.vals <- d.vals * weights
  
  degdat <- resp.data[,attribute.names]
  degdat$.weights <- weights
  degdat$.d.val <- d.vals

  ## it took a while to figure out the as.quoted(...) line,
  ## but see ?. for some limited help on constructing other
  ## expressions like this one
  deg.tot <- ddply(degdat,
                   as.quoted(attribute.names),
                   .fun=function(x) {
                     if (nrow(x)==0) {
                       ## TODO -- think about this; i think we should
                       ## actually return 0 here...
                       ##return(data.frame(mean.degree=NA,
                       ##                  sum.degree=NA))
                       return(data.frame(mean.degree=0,
                                         sum.degree=0,
                                         wgt.num.obs=0,
                                         num.obs=0))                       
                     } else {
                       ##res.sum <- sum(x[,'.weighted_degree'],na.rm=TRUE)
                       ##res.mean <- sum(x[,'.weighted_degree'],na.rm=TRUE)
                       touse <- ! is.na(x$.d.val)
                       res.sum <- sum(x$.d.val[touse] * x$.weights[touse])
                       ## this is a Hajek-type estimate for the mean degree
                       res.mean <- sum(x$.d.val[touse] * x$.weights[touse])/
                                   sum(x$.weights[touse])
                       res.wgt.sum <- sum(x$.weights[touse])

                       return(data.frame(mean.degree=res.mean,
                                         sum.degree=res.sum,
                                         wgt.num.obs=res.wgt.sum,
                                         num.obs=nrow(x)))
                     }
                    },
                   .drop=FALSE)  

  return(deg.tot)

}
