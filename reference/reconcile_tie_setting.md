# Reconcile a value declared on a tie with one set on a rule or argument

Internal. Returns the value to use, or stops when two explicitly-set
values disagree. Silent precedence is deliberately not offered: quietly
preferring one source would produce a number computed under an
assumption the caller did not know was in force, which is the failure
this whole layer exists to prevent.

## Usage

``` r
reconcile_tie_setting(
  what,
  tie.value,
  other.value,
  other.declared,
  other.where,
  default
)
```

## Arguments

- what:

  name of the setting, for the message

- tie.value:

  value declared on the `tie_config`, or `NULL`

- other.value:

  value set elsewhere

- other.declared:

  was `other.value` actually set, or just a default?

- other.where:

  human-readable description of where `other.value` came from

- default:

  value to use when neither source declared one

## Value

the resolved value
