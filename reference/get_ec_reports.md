# get ego X cell reports

Produces a dataframe that has a row for each respondent X cell
containing respondent's reported deaths and exposure among siblings in
the cell.

## Usage

``` r
get_ec_reports(
  esc.dat,
  ego.id,
  sib.dat,
  sib.frame.indicator,
  cell.vars,
  weights,
  ind.vis.var = NULL,
  discretize.exp = FALSE,
  check.frame.consistency = TRUE
)
```

## Arguments

- esc.dat:

  Dataset with a row for each respondent X sibling X cell, likely
  produced by
  [`get_esc_reports`](http://dennisfeehan.org/networkreporting/reference/get_esc_reports.md)

- ego.id:

  String with the name of the column in `esc.dat` containing the survey
  respondent's id

- sib.dat:

  Dataset with a row for each reported sibling, likely produced by TODO

- sib.frame.indicator:

  String with the name of the column in `sib.dat` containing a 0/1 coded
  variable indicating whether or not each sib is in the frame population

- cell.vars:

  a vector with Strings containing the names of columns in `esc.dat`
  identifying the cells to group reports by (typically age group, sex,
  time period)

- weights:

  String with the name of the column in `esc.dat` that has the survey
  weights

- ind.vis.var:

  String with the name of the column in `esc.dat` that has the
  individual visibility for each sibling; defaults to NULL. If NULL, the
  individual visibilities are calculated

- discretize.exp:

  Boolean for whether or not expsoure should be discretized. Not yet
  implemented.

- check.frame.consistency:

  Boolean; if TRUE (the default), warn when any occurrence is attributed
  to an alter marked as being in the frame population. For mortality
  that is impossible and indicates a miscoded frame indicator. Set FALSE
  when the event being counted is not death.

## Value

A dataframe that has a row for each respondent X cell containing
respondent's reported deaths and exposure among siblings in the cell.

## Examples

``` r
  # TODO - add example
```
