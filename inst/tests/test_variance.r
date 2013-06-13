## TODO -- test that there is some variance across a set of bootstrap
##         samples

## TODO -- for ratios, test also that there is some variance in
##         numerator and denominator

## TODO -- test paired ego / alter datasets

## TODO -- test that calling bootstrap.estimates works when
##         total.popn.size is an argument and not an attribute of
##         the data frame (had to use parent.frame(2)) to fix
##         a bug about this

#########################################
## simple random sample (SRS) bootstrap
context("variance estimators - srs bootstrap")

## TODO -- test that mean works


#########################################
## rescaled (Rao / Wu) bootstrap
context("variance estimators - rescaled bootstrap")

## think about how to test the rescaled weights
## think about different sampling designs to test:
##    0, 1, 2, 3 strata
##    1, 2, 10 clusters (?)
##    others?






