# multiplicity.estimator

compute multiplicity estimate of the population size

## Usage

``` r
multiplicity.estimator(survey.data, mult.col = "mult")
```

## Arguments

- survey.data:

  the dataframe with the survey results

- mult.col:

  the name or index of the column that contains, for each respondent,
  the individual value of the number known divided by the sum of the
  multiplicities (TODO MORE DETAIL)

## Value

the multiplicity estimate of the hidden population's size (as a
prevalence)
