# nsum.estimator

compute network scale-up (nsum) estimate of the hidden population's
size. if the degree ratio and information transmission rate are both 1
(the defaults), this is the Killworth estimator.

## Usage

``` r
nsum.estimator(
  survey.data,
  d.hat.vals = "d",
  d.tot.hat = NULL,
  y.vals = "y",
  total.popn.size = NULL,
  deg.ratio = 1,
  tx.rate = 1,
  weights = NULL,
  killworth.se = FALSE,
  missing = "ignore",
  verbose = FALSE,
  ...
)
```

## Arguments

- survey.data:

  the dataframe with survey results

- d.hat.vals:

  the name or index of the column that contains each respondent's
  estimated degree; note that if d.total.hat is specified, then this
  argument (d.hat.vals) is ignored

- d.tot.hat:

  if not NULL, then the estimated total degree of the population (i.e.,
  the average degree times the number of people); if NULL, then use
  d.hat.vals (see above)

- y.vals:

  the name or index of the column that contains the count of hidden popn
  members known

- total.popn.size:

  NULL, NA, or a size

- deg.ratio:

  the degree ratio, \frac\bard_T\bard; defaults to 1

- tx.rate:

  the information transmission rate; defaults to 1

- weights:

  if not NULL, weights to use in computing the estimate. this should be
  the name of the column in the survey.data which has the variable with
  the appropriate weights. these weights should be construted so that,
  eg, the mean of the degrees is estimated as (1/n) \* \sum_i w_i \* d_i

- killworth.se:

  if not NA, return the Killworth et al estimate of

- missing:

  if "ignore", then proceed with the analysis without doing anything
  about missing values. if "complete.obs" then only use rows that have
  no missingness for the computations (listwise deletion). care must be
  taken in using this second option

- verbose:

  if TRUE, print messages to the screen

- ...:

  extra parameters to pass on to the bootstrap fn, if applicable

## Value

the nsum estimate of the hidden population's size (as a prevalence or an
absolute number, depending on total.popn.size)

## Details

TODO – cite Killworth estimator, our methods paper TODO – add refs to
deg ratio and tx rate stuff...
