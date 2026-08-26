# get a dataset with reports used for internal-consistency checks

get a dataset with reports used for internal-consistency checks

## Usage

``` r
get_ic_reports(
  esc.dat,
  ego.dat,
  ego.id,
  sib.id,
  sib.frame.indicator,
  sib.cell.vars,
  ego.cell.vars
)
```

## Arguments

- esc.dat:

  The ego X sibling X cell dataset (see
  [get_esc_reports](http://dennisfeehan.org/networkreporting/reference/get_esc_reports.md))

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

## Value

A tibble with a row for each reported sibling in the frame population,
the sibling's cell info, the cell info of the survey respondent who
reported each sibling, and unified variables with the ego and sibs' cell
values; these unified variables are called `.ego.cell` and `.sib.cell`,
and are used in
[sib_ic_checks](http://dennisfeehan.org/networkreporting/reference/sib_ic_checks.md).

## Details

The `sib.cell.vars` and `ego.cell.vars` arguments should have a vector
with Strings containing the names of columns in `esc.dat` identifying
the cells to group reports by (typically age and sex, and possibly other
variables). Note that, unlike the `cell.vars` argument in
[get_ec_reports](http://dennisfeehan.org/networkreporting/reference/get_ec_reports.md),
for this function `sib.cell.vars` and `ego.cell.vars` MUST have the age
variable listed FIRST. The goal of `get_ic_reports` is to find the cell
that each sibling was in at the time of the interview, meaning the
oldest age group to which the sibling contributed any exposure.

## Examples

``` r
  # TODO write example code
```
