# Summarise how a set of visibilities was arrived at

Summarise how a set of visibilities was arrived at

## Usage

``` r
vis_provenance(
  rule,
  values,
  esc.dat,
  tie = NULL,
  dropped.tiers = NULL,
  ego.in.group = NA,
  frame.indicator = NA_character_
)
```

## Arguments

- rule:

  the
  [visibility_rule](http://dennisfeehan.org/networkreporting/reference/visibility_rule.md)
  that produced `values`

- values:

  the tibble returned by the rule's
  [`predict()`](https://rdrr.io/r/stats/predict.html)

- esc.dat:

  the report data the rule was applied to

## Value

a `vis_provenance` object
