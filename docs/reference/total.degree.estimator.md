# total.degree

estimate the total degree of the population network from sample degrees

## Usage

``` r
total.degree.estimator(
  survey.data,
  d.hat.vals = "d",
  weights = NULL,
  missing = "ignore"
)
```

## Arguments

- survey.data:

  the dataframe with survey results

- d.hat.vals:

  the name or index of the column that contains each respondent's
  estimated degree

- weights:

  if not NULL, weights to use in computing the estimate. this should be
  the name of the column in the survey.data which has the variable with
  the appropriate weights. these weights should be construted so that,
  eg, the mean of the degrees is estimated as (1/n) \* \sum_i w_i \* d_i

- missing:

  if "ignore", then proceed with the analysis without doing anything
  about missing values. if "complete.obs" then only use rows that have
  no missingness for the computations (listwise deletion). care must be
  taken in using this second option

## Value

the estimated total degree

## Details

this computes the weighted sum of the respondents' estimated degrees.  
' TODO – for now, it doesn't worry about missing values OR about
differences between the frame and the universe
