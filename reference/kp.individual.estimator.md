# Individual personal network size estimates using the known population method

In most situations, the known population method will be used to estimate
the average personal network size; this can be done with
[`kp.estimator_`](http://dennisfeehan.org/networkreporting/reference/kp.estimator.md).
If, instead, you wish to estimate the personal network size of each
individual respondent, then you can use this function.

## Usage

``` r
kp.individual.estimator(
  resp.data,
  known.populations,
  total.kp.size = 1,
  alter.popn.size,
  dropmiss = FALSE
)

kp.individual.estimator_(
  resp.data,
  known.populations,
  total.kp.size = 1,
  alter.popn.size,
  dropmiss = FALSE
)
```

## Arguments

- resp.data:

  the respondent (survey) data

- known.populations:

  the names of the known populations

- total.kp.size:

  the sum of the sizes of all of the known populations

- alter.popn.size:

  the size of the population respondents are reporting about connections
  to; typically this will be the frame population, so `alter.popn.size`
  should be the size of the frame population, N.F

- dropmiss:

  see the dropmiss argument of
  [`kp.estimator_`](http://dennisfeehan.org/networkreporting/reference/kp.estimator.md)

## Value

a data frame with an estimate of each individual respondent's personal
network size

## Details

Note that this is not making inference about any larger population; it
estimates a property of each individual respondent. So the sampling
weights are not used here.

## TODO

- make unit tests
