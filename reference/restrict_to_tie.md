# Restrict a coalesced rule to the tiers valid for a declared tie

Internal. Returns the rule unchanged when it has no tiers or all of them
apply; otherwise a rebuilt chain of the applicable ones, or the single
applicable rule. Attaches the dropped tier labels as an attribute so
that provenance can report them.

## Usage

``` r
restrict_to_tie(rule, tie)
```

## Arguments

- rule:

  a `visibility_rule`

- tie:

  a `tie_config`

## Value

a `visibility_rule`
