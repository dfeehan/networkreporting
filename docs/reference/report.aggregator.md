# aggregate a reported quantity by groups

This function takes a quantity and aggregates it by groups, using the
design weights.

## Usage

``` r
report.aggregator_(
  resp.data,
  attribute.names,
  qoi,
  weights,
  qoi.name,
  scaling.factor = NULL,
  dropmiss = FALSE
)

report.aggregator(
  resp.data,
  attribute.names = NULL,
  qoi,
  weights,
  qoi.name = NULL,
  scaling.factor = NULL,
  dropmiss = FALSE
)
```

## Arguments

- resp.data:

  the data

- attribute.names:

  the names of the variables that define the groups for which the qoi
  should be aggregated

- qoi:

  the variable with quantity to aggregate

- weights:

  analysis weights; either the name of a column that has sampling
  weights or a vector with the names of columns of the dataset that have
  bootstrap weights. Currently, these weights must be named
  "boot_weight_1", "boot_weight_2", ...

- qoi.name:

  the name of the qoi

- scaling.factor:

  a factor by which weights should be multiplied before applying them.
  Defaults to NULL (no scaling)

- dropmiss:

  if TRUE, then drop missing values and rescale the weights to preserve
  their total. So, if weights sum to 100, and dropping rows with missing
  values leads to weights that sum to 80, then the remaining rows will
  have their weights multiplied by (100/80) to ensure the weights still
  add up to 100 after dropping the rows with missing values. Defaults to
  FALSE

## Value

the aggregated reported quantities
