# Can a rule's visibility be predicted from ego X cell data?

Internal. Decides between the two bootstrap paths for an estimated rule.
The cheap path recomputes visibility-adjusted sums from the frame-split
statistics in `ec.dat`, which is valid only when the rule's prediction
is constant within an ego X cell row — equivalently, when everything the
rule needs is present in `ec.dat`.

## Usage

``` r
vis_is_cell_constant(rule, ec.dat)
```

## Arguments

- rule:

  a `visibility_rule`

- ec.dat:

  the ego X cell data

## Value

`TRUE` if the cheap per-cell path applies

## Details

A rule matching on a covariate that is also a cell variable satisfies
this, which is the common case. One matching on something that cuts
across cells, or a model with continuous predictors, does not.
