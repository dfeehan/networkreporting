# Average personal network size estimates using known population method

If given `attribute.names`, then this function produces estimated
average network sizes given by the groups that are defined by all
combinations of the attributes; otherwise, it estimates the average
personal network size for the entire frame population.

## Usage

``` r
kp.estimator_(
  resp.data,
  known.populations,
  weights,
  boot.weights = NULL,
  ego.id = NULL,
  return.boot = FALSE,
  attribute.names = NULL,
  total.kp.size = NULL,
  alter.popn.size = NULL,
  dropmiss = FALSE,
  verbose = TRUE
)

kp.estimator(
  resp.data,
  known.populations,
  weights,
  attribute.names = NULL,
  total.kp.size = 1,
  alter.popn.size = NULL,
  dropmiss = FALSE
)
```

## Arguments

- resp.data:

  the dataframe that has the survey responses

- known.populations:

  the names of the columns in `resp.data` that have respondents' reports
  about connections to known populations

- weights:

  the name of a column that has sampling weights

- boot.weights:

  Optional dataframe with bootstrap resampled weights. See Details for
  more info.

- ego.id:

  If boot.weights are included, then this is the name of the column(s)
  we need to join the bootstrap weights onto the dataset. This is most
  often the id of the ego making the reports.

- return.boot:

  if TRUE and boot.weights are included, then return the full
  bootstrapped estiamates and not just the summaries; this option causes
  this function to return a list instead of a tibble

- attribute.names:

  the names of the columns in `resp.data` that determine the subgroups
  for which average degree is estimated; if NULL, then the average over
  all respondents is estimated

- total.kp.size:

  the size of the probe alters; i.e., the sum of the known population
  sizes. if NULL, then this is set to 1

- alter.popn.size:

  the size of the population of alters; this is most often the frame
  population, which is the default if nothing else is specified; the
  size of the frame population is taken to be the sum of the weights
  over all of resp.data

- dropmiss:

  if FALSE, then, for each row, use only the reports about connections
  to known populations that have no missingness. This effectively
  assumes that missing reports are 0. if TRUE, then only use rows that
  have no missingness in reported connections to known populations in
  estimating degree. in this case, the sampling weights are rescaled so
  that the implied total size of the frame population is not changed.
  (see the 'dropmiss' argument to the function report.aggregator\_)
  future versions may have other options

- verbose:

  if TRUE, print information to screen

## Value

the estimated average degree (`dbar.Fcell.F`) for respondents in each of
the categories given by `attribute.names`

## Technical note

The estimated average degree is \\(\sum y\_{F\_\alpha, A} / N_A) \times
N_F / N\_{F\_\alpha}\\ here, we estimate \\N_F / N\_{F\_\alpha}\\ by
dividing the total of all respondents' weights by the sum of the weights
for respondents in each cell \\\alpha\\.

## TODO

- make unit tests

- think about how to elegantly add options for dbar\_(P,Q) vs
  dbar\_(Q,P)

## Details

If you want estimated sampling variances, you can pass in a data frame
`boot.weights`. `boot.weights` is assumed to have a column that is named
whatever the `ego.id` is, and then a series of columns named
`boot_weight_1`, ..., `boot_weight_M`.

The two options for missing values are 'ignore' or 'complete.obs'.
'ignore' adds up each respondent's nonmissing reported connections to
the known populations, effectively treating missing reports as 0s.
'complete.obs' only uses responses from respondents who have non-missing
values for all of the known population reports.
