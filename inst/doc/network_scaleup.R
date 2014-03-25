
## @knitr , message=FALSE
library(networkreporting)
library(plyr)
library(ggplot2) # we'll use qplot from ggplot2 for plots
theme_set(theme_minimal())

data(hhsurvey) # this is a demo dataset included with the package


## @knitr 
head(example.survey)


## @knitr 
head(knownpop.dat)


## @knitr 
kp.vec <- knownpop.dat$size
names(kp.vec) <- paste(knownpop.dat$known.popn)

d.hat <- kp.degree.estimator(survey.data=example.survey,
                             known.popns=kp.vec,
                             total.popn.size=tot.pop.size,
                             missing="complete.obs")


## @knitr 
qplot(d.hat, binwidth=25)


## @knitr 
example.survey$d.hat <- d.hat


## @knitr 
iv.result <- nsum.internal.validation(survey.data=example.survey,
                                      known.popns=kp.vec,
                                      missing="complete.obs",
                                      killworth.se=TRUE,
                                      total.popn.size=tot.pop.size,
                                      kp.method=TRUE,
                                      return.plot=FALSE)


## @knitr 
iv.result$results


## @knitr 
print(iv.result$plot)


## @knitr 
idu.est <- nsum.estimator(survey.data=example.survey,
                          d.hat.vals=d.hat,
                          total.popn.size=tot.pop.size,
                          y.vals="idu",
                          missing="complete.obs")


## @knitr 
idu.est


## @knitr , tidy=FALSE
idu.est <- bootstrap.estimates(## this describes the sampling design of the
                               ## survey; here, the PSUs are given by the
                               ## variable cluster, and the strata are given
                               ## by the variable region
                               survey.design = ~ cluster + strata(region),
                               ## the number of bootstrap resamples to obtain
                               num.reps=100,
                               ## this is the name of the function
                               ## we want to use to produce an estimate
                               ## from each bootstrapped dataset
                               estimator.fn="nsum.estimator",
                               ## these are the sampling weights
                               weights="indweight",
                               ## this is the name of the type of bootstrap
                               ## we wish to use
                               bootstrap.fn="rescaled.bootstrap.sample",
                               ## our dataset
                               survey.data=example.survey,
                               ## other parameters we need to pass
                               ## to the nsum.estimator function
                               d.hat.vals=d.hat,
                               total.popn.size=tot.pop.size,
                               y.vals="idu",
                               missing="complete.obs")


## @knitr 
## combine the estimates together in one data frame
## (bootstrap.estimates gives us a list)
all.idu.estimates <- ldply(idu.est,
                           function(x) { data.frame(estimate=x$estimate) })


## @knitr 
## look at a histogram of the results
qplot(all.idu.estimates$estimate, binwidth=50)

## summarize the results
summary(all.idu.estimates$estimate)


