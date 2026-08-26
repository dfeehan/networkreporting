# Assumptions the clique rule makes, given whether ego is in the group

Internal. Computed from a value rather than fixed at construction,
because a
[`tie_config()`](http://dennisfeehan.org/networkreporting/reference/tie_config.md)
may override `ego.in.group` after the rule is built.

## Usage

``` r
clique_assumptions(ego.in.group)
```

## Arguments

- ego.in.group:

  is ego a member of the group ego reports about?

## Value

a character vector of assumptions
