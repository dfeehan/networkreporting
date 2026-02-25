# rdsII.estimator

compute an estimate for the prevalence of a trait from an RDS sample,
using the estimator described in TODO \[Volz + Heckathorn '08\]

## Usage

``` r
rdsII.estimator(
  survey.data,
  d.hat.vals,
  y.vals,
  missing = "ignore",
  verbose = FALSE
)
```

## Arguments

- survey.data:

  the dataframe with RDS survey results

- d.hat.vals:

  the name or index of the column that contains each respondent's
  estimated degree

- y.vals:

  the name or index of the column that contains the quantity of
  interest. if this is a dichotomous trait, it should be 0 / 1

- missing:

  if "ignore", then proceed with the analysis without doing anything
  about missing values. if "complete.obs" then only use rows that have
  no missingness for the computations (listwise deletion). care must be
  taken in using this second option

- verbose:

  if TRUE, print messages to the screen

## Value

the RDS-II estimate of the average of the quantity of interest

## Details

NOTE: we have no weights for now, right? RDS doesn't get used with
weights?
