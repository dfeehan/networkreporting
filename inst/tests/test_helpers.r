


## tests for get.var and get.weights
context("helpers - get.var")

gv.tmp <- data.frame('A'=1:5, 'B'=5:1, weights=10:14)

expect_that(get.var(gv.tmp, 'A'), equals(1:5))
expect_that(get.var(gv.tmp, 1), equals(1:5))

expect_that(get.var(gv.tmp, 'B'), equals(5:1))
expect_that(get.var(gv.tmp, 2), equals(5:1))

expect_that(get.var(gv.tmp, NULL), equals(rep(NA,5)))
expect_that(get.var(gv.tmp, NULL, default=-1), equals(rep(-1,5)))

expect_that(get.var(gv.tmp, 10), throws_error())

expect_that(get.weights(gv.tmp, "weights"), equals(10:14))
expect_that(get.weights(gv.tmp, NULL), equals(rep(1,5)))

## TODO -- write test for df.to.kpvec


## TODO -- write test for parse_design

## TODO -- write test for topcodev.var
## TODO -- write test for topcode.data

## TODO -- do we need weighted mean fn?

## TODO -- write test for parse.total.popn.size

## TODO -- write test for estimate.error

## TODO -- write test for attributes.to.long (or, move to dhstools?)

