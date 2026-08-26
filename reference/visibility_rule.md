# Construct a visibility rule

A rule is a closure, not a table, and it is split into `fit` and
`predict`. The split is what lets one interface cover exact derivation,
donor means and (later) a fitted model, and what makes correct
bootstrapping possible: a rule whose `fit` step consumes the sample must
be refit inside each bootstrap replicate, and one whose does not must
not be.

## Usage

``` r
visibility_rule(
  label,
  requires,
  is_estimated,
  fit,
  predict,
  assumptions = character(0),
  params = list(),
  applies_to = NA_character_,
  tie_overridable = character(0),
  declared = list(),
  assumptions_fn = NULL
)
```

## Arguments

- label:

  short string naming the rule; appears in provenance output

- requires:

  character vector of columns the rule needs, checked up front

- is_estimated:

  `TRUE` if `fit()` consumes the sample. Drives the bootstrap path: see
  [`apply_visibility_rule()`](http://dennisfeehan.org/networkreporting/reference/apply_visibility_rule.md)

- fit:

  `function(donor.dat, weights)` returning opaque state

- predict:

  `function(alter.rows, state)` returning one row per row of
  `alter.rows`, with columns `vis`, `vis_weight` and `vis_rule`

- assumptions:

  character vector of assumptions this rule makes, carried into the
  provenance table so they reach output rather than living only in a
  methods appendix

- params:

  list of the constructor's arguments, for printing

## Value

an object of class `visibility_rule`
