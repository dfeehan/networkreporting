# topcode a vector of numerical values

this function topcodes one vector; it's used by the `topcode` function
to topcode a set of columns in a data frame

## Usage

``` r
topcode.var(x, max, to.na = NULL, ignore = NA)
```

## Arguments

- x:

  the vector of values to topcode

- max:

  the maximum value; all values \> max are recoded to max

- to.na:

  a vector of values to recode to NA (this happens before topcoding)

- ignore:

  a vector of values to leave unchanged

## Value

the topcoded vector

## Examples

``` r
if (FALSE) { # \dontrun{
   ## TODO write example
} # }
```
