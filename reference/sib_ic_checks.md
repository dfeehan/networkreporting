# get calculate internal consistency checks for sibling reports

get calculate internal consistency checks for sibling reports

## Usage

``` r
sib_ic_checks(
  esc.dat,
  ego.dat,
  ego.id,
  sib.id,
  sib.frame.indicator,
  sib.cell.vars,
  ego.cell.vars,
  boot.weights
)
```

## Arguments

- esc.dat:

  The ego X sibling X cell dataset (see `get_esc_reports`)

- ego.dat:

  The ego dataset, containing one row for each survey respondent

- ego.id:

  String with the name of the column in `esc.dat` containing the survey
  respondent ID

- sib.id:

  String with the name of the column in `esc.dat` containing the unique
  sibling ID

- sib.frame.indicator:

  String with the name of the column in `sib.dat` containing a 0/1 coded
  variable indicating whether or not each sib is in the frame population

- sib.cell.vars:

  see Details

- ego.cell.vars:

  see Details

- boot.weights:

  Dataframe with bootstrap resampled weights. See Details

## Value

A list with two entries: `ic.summ`, with summarized results; and
`ic.boot.ests` with the full results for each bootstrap rep

## Details

The `sib.cell.vars` and `ego.cell.vars` arguments should have a vector
with Strings containing the names of columns in `esc.dat` identifying
the cells to group reports by (typically age and sex, and possibly other
variables). Note that, unlike the `cell.vars` argument in
`get_ec_reports`, for this function `sib.cell.vars` and `ego.cell.vars`
MUST have the age variable listed FIRST. The goal of `get_ic_reports` is
to find the cell that each sibling was in at the time of the interview,
meaning the oldest age group to which the sibling contributed any
exposure. `boot.weights` is assumed to have a column that is named
whatever the `ego.id` is, and then a series of columns named
`boot_weight_1`, ..., `boot_weight_M`.

## Examples

``` r
  # TODO write example code
```
