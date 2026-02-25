# plot_meanties_truth

plot the relationship between the mean number of ties in the survey
dataset and the true popn sizes

## Usage

``` r
plot_meanties_truth(survey.data, weights = NULL, known.popns = NULL)
```

## Arguments

- survey.data:

  the dataframe with the survey results

- weights:

  if not NULL, weights to use in computing the estimate. this should be
  the name of the column in the survey.data which has the variable with
  the appropriate weights. these weights should be construted so that,
  eg, the mean of the degrees is estimated as (1/n) \* \sum_i w_i \* d_i

- known.popns:

  if not NULL, a vector whose entries are the size of the known
  populations, and whose names are the variable names in the dataset
  corresponding to each one. if NULL, then assume that the survey.data
  dataframe has an attribute called 'known.popns' containing this
  vector.

## Value

a ggplot2 object with the relationship plot

## Details

TODO - more in-depth description of this function
