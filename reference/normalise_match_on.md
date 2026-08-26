# Normalise a `match_on` specification

`match_on` describes the *alter*, but donors are respondents, and the
two frequently spell the same covariate differently (`.sib.sex` on an
alter row, `sex` on an ego row). A named vector expresses the
correspondence: `c(.sib.sex = "sex")` means "the alter column `.sib.sex`
matches the donor column `sex`". An unnamed entry means the column is
spelled the same on both sides.

## Usage

``` r
normalise_match_on(match_on)
```

## Arguments

- match_on:

  character vector, optionally named

## Value

a named character vector, alter column -\> donor column
