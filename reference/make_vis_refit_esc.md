# Build a per-replicate refit that recomputes visibility at the report level

Internal. The expensive bootstrap path, for an estimated rule whose
visibility is *not* constant within an ego X cell row. There the
frame-split identity does not apply, and the only correct route is to
refit the rule and re-predict for every report inside each replicate,
then re-aggregate.

## Usage

``` r
make_vis_refit_esc(
  rule,
  donor.dat,
  boot.weights,
  esc.dat,
  ec.dat,
  cell.vars,
  ego.id = ".ego.id"
)
```

## Arguments

- rule:

  the
  [visibility_rule](http://dennisfeehan.org/networkreporting/reference/visibility_rule.md)

- donor.dat:

  the donor frame

- boot.weights:

  data frame of replicate weights

- esc.dat:

  the ego X alter X cell reports, carrying `ind_vis`'s inputs

- ec.dat:

  the ego X cell data the estimate is computed from

- cell.vars:

  the columns defining a cell

- ego.id:

  name of the ego id column

## Value

`function(r)` returning a two-column data frame of `num` and `denom`,
one row per row of `ec.dat`, or `NULL` if the rule is not estimated

## Details

Costs roughly M times a point estimate. That is the price of not
freezing a sample quantity; falling back to the cheap path here would
silently reinstate the very bug the refit exists to prevent.
