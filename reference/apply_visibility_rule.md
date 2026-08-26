# Apply a visibility rule to ego X alter X cell reports

Validates the rule's `requires` against the columns actually present,
fits the rule, predicts a visibility for every row, and returns both the
values and a provenance table describing how they were arrived at.

## Usage

``` r
apply_visibility_rule(
  rule,
  esc.dat,
  ego.dat = NULL,
  sib.dat = NULL,
  ego.id = ".ego.id",
  frame.indicator = NULL,
  weights = NULL,
  ego.in.group = NULL,
  tie = NULL
)
```

## Arguments

- rule:

  a
  [visibility_rule](http://dennisfeehan.org/networkreporting/reference/visibility_rule.md)

- esc.dat:

  ego X alter X cell reports, one row per report

- ego.dat:

  ego-level data, used as the donor frame when a rule asks for
  `donor = "egos"`. Needs a `y.F` column; if it has none and `sib.dat`
  is supplied, one is derived

- sib.dat:

  long-form alter data, used to derive `y.F` when `esc.dat` or `ego.dat`
  lacks it

- ego.id:

  name of the ego id column

- frame.indicator:

  name of the 0/1 frame membership column. `NULL` (the default) takes it
  from `tie`, falling back to `".sib.in.F"`. Setting it here as well as
  on the tie is an error if the two disagree

- weights:

  name of the column holding donor sampling weights

- ego.in.group:

  is ego a member of the group ego reports about? Governs how `y.F` is
  derived. `NULL` (the default) takes it from `tie`, falling back to
  `TRUE`. Setting it here as well as on the tie is an error if the two
  disagree; the tie is where it belongs

- tie:

  a
  [`tie_config()`](http://dennisfeehan.org/networkreporting/reference/tie_config.md)
  saying what kind of tie these reports are about. Required when `rule`
  assumes a tie structure —
  [`vis_from_clique()`](http://dennisfeehan.org/networkreporting/reference/vis_from_clique.md)
  does — because applicability cannot be read off the data: on a tie
  that is not a clique the clique rule still returns a finite, plausible
  number, and it is wrong. A rule that makes no structural assumption,
  such as
  [`vis_from_donor()`](http://dennisfeehan.org/networkreporting/reference/vis_from_donor.md),
  needs no `tie`. When `rule` is a
  [`vis_coalesce()`](http://dennisfeehan.org/networkreporting/reference/vis_coalesce.md)
  chain, tiers inapplicable to `tie` are dropped, and named in the
  returned provenance.

## Value

a list with `values` (a tibble of `vis`, `vis_weight`, `vis_rule`, one
row per row of `esc.dat`) and `provenance` (a `vis_provenance` tibble)
