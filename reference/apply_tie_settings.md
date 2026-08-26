# Apply a tie\\s declared settings to a fitted rule state

Internal. A rule names, in `tie_overridable`, the state entries a tie
may set. Handles a coalesced rule by descending into each tier.

## Usage

``` r
apply_tie_settings(rule, state, tie)
```

## Arguments

- rule:

  the rule

- state:

  the state returned by the rule\\s `fit()`

- tie:

  a `tie_config`, or `NULL`

## Value

`state`, with tie-declared settings applied
