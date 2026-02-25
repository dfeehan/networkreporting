# topcode a group of variables

this function uses `topcode.var` to topcode a set of variables. it's
useful for topcoding a whole set of aggregated relational data ("how
many X are you connected to?") questions in the same way.

## Usage

``` r
topcode.data(survey.data, vars, max, to.na = NULL, ignore = NA)
```

## Arguments

- survey.data:

  the dataset with the survey responses

- vars:

  a vector with the names or indices of the columns in the dataframe
  that are to be topcoded

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
   data(hh.survey) # example data included with the package
   example.survey <- topcode.data(example.survey,
                                  vars=known.popn.vars,
                                  max=30)
} # }
```
