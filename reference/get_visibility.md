# calculate visibility for each sibship and ego

calculate visibility for each sibship and ego

## Usage

``` r
get_visibility(
  ego.dat,
  ego.id,
  sib.dat,
  sib.frame.indicator,
  weight = "wwgt",
  age = "age.cat"
)
```

## Arguments

- ego.dat:

  The ego dataset (likely produced by
  `siblingsurvival::prep_dhs_sib_histories()`)

- ego.id:

  String with the name of the column in `sib.dat` containing the survey
  respondent ID

- sib.dat:

  The long-form sibling dataset (likely produced by
  `siblingsurvival::prep_dhs_sib_histories()`)

- sib.frame.indicator:

  String with the name of the column in `sib.dat` containing a 0/1 coded
  variable indicating whether or not each sib is in the frame population

- weight:

  string with the name of the column in `ego.dat` and `sib.dat`
  containing the sampling weight. Defaults to `wwgt`

- age:

  string with the name of the column in `ego.dat` containing the age
  group. Defaults to `age.cat`

## Value

A list with three entries:

- `ego_vis` - a tibble with one row per ego and the ego-specific
  visibilities

- `ego_vis_agg` - a tibble with per-(sex, age) visibility summaries

- `sib_res` - a tibble with one row per reported sibling, along with
  tibble with a row for each survey respondent (each unique value of
  `ego.id`), and the number of sibs the respondent reported on the
  frame, including and not including herself

## Examples

``` r
  # TODO write example code
```
