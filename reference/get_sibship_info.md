# calculate number of sibs on frame for each respondent

this quantity, y.F, is related to the visibility of each respondent

## Usage

``` r
get_sibship_info(sib.dat, ego.id, sib.frame.indicator)
```

## Arguments

- sib.dat:

  The long-form sibling dataset (likely produced by a prep function such
  as `siblingsurvival::prep_dhs_sib_histories()`)

- ego.id:

  String with the name of the column in `sib.dat` containing the survey
  respondent ID

- sib.frame.indicator:

  String with the name of the column in `sib.dat` containing a 0/1 coded
  variable indicating whether or not each sib is in the frame population

## Value

A tibble with a row for each survey respondent (each unique value of
`ego.id`), and the number of sibs the respondent reported on the frame,
including and not including herself

## Examples

``` r
  # TODO write example code
```
