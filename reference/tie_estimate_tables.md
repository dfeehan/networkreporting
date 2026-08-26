# Pull the estimate table out of a set of estimator results

Internal. Validates that the inputs look like estimator results, are
named, and share their cell columns.

## Usage

``` r
tie_estimate_tables(results, estimator = c("ind", "agg"), boot = FALSE)
```

## Arguments

- results:

  a named list of results from
  [`network_survival_estimator()`](http://dennisfeehan.org/networkreporting/reference/network_survival_estimator.md)

- estimator:

  `"ind"` or `"agg"`

- boot:

  pull the replicate-level table instead of the summary

## Value

a named list of tibbles
