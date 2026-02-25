# kp.degree.estimator (DEPRECATED)

see
[`kp.individual.estimator`](http://dennisfeehan.org/networkreporting/reference/kp.individual.estimator.md)
instead.

## Usage

``` r
kp.degree.estimator(
  survey.data,
  known.popns = NULL,
  total.popn.size = NULL,
  dropmiss = FALSE,
  verbose = FALSE
)
```

## Arguments

- survey.data:

  the dataframe with the survey results

- known.popns:

  if not NULL, a vector whose entries are the size of the known
  populations, and whose names are the variable names in the dataset
  corresponding to each one. if NULL, then assume that the survey.data
  dataframe has an attribute called 'known.popns' containing this
  vector.

- total.popn.size:

  the size of the entire population. if NULL, this function returns
  proportions; if not NULL, it returns absolute numbers (ie, the
  proportions \* total popn size)

- dropmiss:

  if "ignore", then proceed with the analysis without doing anything
  about missing values. if "complete.obs" then, for each row, use only
  the known populations that have no missingness for the computations.
  care must be taken in using this second option

- verbose:

  if TRUE, print messages to the screen

## Value

a vector with an estimate of the degree for each row in survey.data. if
missing=="ignore", then the degree for rows that have missingness in the
'how many X' questions will be set to NA

## Details

compute an estimate of the respondents' degrees using the known
population method  

note that this function does not take survey weights, since these
estimates are not for total degree, but just for the individual degree
of each respondent
