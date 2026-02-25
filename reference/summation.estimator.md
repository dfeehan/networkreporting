# summation.estimator

compute an estimate of the respondents' degrees using the summation
method

## Usage

``` r
summation.estimator(survey.data, sum.q = NULL, missing = "ignore")
```

## Arguments

- survey.data:

  the dataframe with the survey results

- sum.q:

  if not NULL, a vector whose entries are the variable names in the
  dataset corresponding to each summation question. if NULL, then assume
  that the survey.data dataframe has an attribute called 'sum.qs'
  containing this vector.

- missing:

  if "ignore", then proceed with the analysis without doing anything
  about missing values. other options are not yet implemented.

## Value

a vector with an estimate of the degree for each row in survey.data. if
na.rm=TRUE, then the degree for rows that have missingness in the
summation questions will be set to NA

## Details

TODO – cite summation method ref

Note that the summation degree estimator for the case where there is
missing data is not yet implemented. (In fact, I don't think that there
is a known estimator for this case.)
