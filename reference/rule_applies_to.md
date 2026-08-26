# Which rules may be applied to which ties

Internal. `applies_to` on a rule is a character vector of structures, or
`NA_character_` meaning the rule makes no structural assumption and is
valid anywhere.

## Usage

``` r
rule_applies_to(rule, tie)
```

## Arguments

- rule:

  a `visibility_rule`

- tie:

  a `tie_config`

## Value

`TRUE` if the rule may be applied to this tie
