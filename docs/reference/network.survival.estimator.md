# network survival estimator

use an aggregate multiplicity estimator and the respondents' own network
size estimates to estimate hidden population sizes

## Usage

``` r
network.survival.estimator_(
  resp.data,
  attribute.data,
  attribute.names,
  known.populations,
  total.kp.size = 1,
  weights,
  attribute.weights,
  within.alter.weights = NULL,
  boot.weights = NULL,
  ego.id = NULL,
  return.boot = FALSE,
  dropmiss = FALSE,
  verbose = TRUE
)

network.survival.estimator(
  resp.data,
  attribute.data,
  attribute.names,
  known.populations,
  total.kp.size = 1,
  weights,
  attribute.weights,
  within.alter.weights = NULL,
  boot.weights = NULL,
  ego.id = NULL,
  return.boot = FALSE,
  dropmiss = NULL,
  verbose = TRUE
)
```

## Arguments

- resp.data:

  The dataframe that has a row for each respondent, with reported
  connections to the groups of known size, as well as the attributes.
  Note that the column names of the attributes should match their names
  in `attribute.data`

- attribute.data:

  A dataframe with the reported attributes of hidden population members
  reported by survey respondents. There should be one row for each time
  a respondent reports a hidden population member. For example, to
  estimate death rates, there should be one row for each report of a
  death.

- attribute.names:

  The names of the columns of attribute.data and resp.data that contain
  the attribute information.

- known.populations:

  The names of the columns in `resp.data` that have responses to the
  known population questions

- total.kp.size:

  The size of the probe alters, i.e., the sum of the known population
  sizes

- weights:

  The weights or weights column for the respondent data

- attribute.weights:

  The weights or weights column for the alter data (this should
  typically be the weight of the respondent who reported the alter)

- within.alter.weights:

  The weight or weights column for within-alter weights. This could be
  useful if, for example, respondents only report about a subset of all
  of their alters. See Details.

- boot.weights:

  Optional dataframe with bootstrap resampled weights. See Details for
  more info.

- ego.id:

  If boot.weights are included, then this is the name of the column(s)
  we need to join the bootstrap weights onto the dataset. This is most
  often the id of the ego making the reports.

- return.boot:

  If TRUE, and if `boot.weights` is specified, then return each
  bootstrap estimate

- dropmiss:

  How to handle missingness in reported connections to known populations
  and number of deaths. See
  [`report.aggregator`](http://dennisfeehan.org/networkreporting/reference/report.aggregator.md)

- verbose:

  If TRUE, print information to screen

## Value

the network reporting estimate of the hidden population's size (as a
prevalence) broken down by the categories defined by all combinations of
`attribute.names`.

## Details

This function takes two sources of data as input: first, it requires a
long-form dataframe with the attributes of the reported members of the
hidden population. For example, if we are asking about emigres and we
collect the age and sex of each reported emigrant, then the long form
dataset might look like:

|     |     |        |
|-----|-----|--------|
| age | sex | weight |
| 15  | m   | 2.10   |
| 58  | f   | 1.15   |
| 33  | m   | 3.67   |

The second source of data we need is the known population responses for
the respondents, along with the \*same\* attributes for each respondent.
For example, in the situation above, we would also require a dataset
like this to be passed in

|     |     |        |             |           |     |
|-----|-----|--------|-------------|-----------|-----|
| age | sex | weight | hm.teachers | hm.nurses | ... |
| 20  | f   | 2.10   | 4           | 0         | ... |
| 44  | m   | 1.65   | 0           | 2         | ... |
| 60  | m   | 2.75   | 1           | 1         | ... |

## Technical note

This function assumes that the sampling weights are standard analysis
weights and \*not\* relative weights. Standard analysis weights should
provide an estimate for the size of the frame population when added up;
relative weights, on the other hand, will sum to the number of
respondents in the sample. Demographic and Health surveys typically have
relative weights, which must be converted into standard sampling weights
before using this function.

## Returned dataframe

Currently, the dataframe that is returned has columns for the attributes
that death rates were calculated for, along with:

- `num.obs.deaths` - number of deaths reported

- `num.obs.degree` - number of respondents in cell used for estimating
  degree

- `y.F.Dcell.hat` - estimated total number connections from the frame to
  deaths

- `y.Fcell.kp.hat` - estimated total number of connections from (frame
  intersect cell) to the groups of known size

- `total.kp.size` - the total size of the groups of known size

- `N.Fcell.hat` - estimated size of (frame intersect cell) population
  (based on survey weights)

- `N.F.hat` - estimated size of frame population (based on survey
  weights)

- `asdr.hat` - the estimated death rate

## TODO

- handle missing values

- allow passing in N.F (currently, we always estimate from weights)

- write more general agg mult est fn and call that

- make unit tests

## Details

If you want estimated sampling variances, you can pass in a data frame
`boot.weights`. `boot.weights` is assumed to have a column that is named
whatever the `ego.id` is, and then a series of columns named
`boot_weight_1`, ..., `boot_weight_M`.

`ego.id` can either be a string or vector of strings, or it can be a
named vector like `c('a'='b')`. In the second case, `'a'` should be the
name of the id column in `resp.data`, while `'b'` should be the name of
the id column in `'attribute.data'`.

`within.alter.weight` is a weight for alters within a respondent. This
is set to NULL by default, and many applications will not need it.
However, it can be helpful if respondents are only asked to report about
a subset of their alters. For example, suppose that respondents can
report about 10 alters in detail. Respondent A reports connections to 3
deaths, and respondent B reports connections to 12 deaths. When
aggregating Respondent A's detailed reports, we would only need to use
the sampling weight. However, if we only used the sampling weight when
aggregating Respondent B's reports, that would imply that Respondent B
only reported connections to 10 deaths. In this case, setting
within.alter.weight to (12/10) for Respondent B will make B's reports
imply that she reported 12 deaths. Note that this makes the assumption
that the 10 deaths Respondent B did report are a uniformly random
subsample of the 12 deaths she reports being connected to.
