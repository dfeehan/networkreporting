# add individual visibility based on sib reports to ego X sib X cell reports

Takes a dataframe that has a row for each respondent X sib X cell and
adds individual visibility to it

## Usage

``` r
add_esc_ind_vis(
  esc.dat,
  ego.id,
  sib.dat,
  sib.frame.indicator,
  varname = "ind_vis"
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

- varname:

  String with the name of the new column to be added to `sib.dat`
  containing the individual visibility

## Value

the ESC dataframe with additional columns that have information about
the sibship size and visibility:

- `ind_vis_weight` - the individual visibility weight

- `y.F` - number of reported sibs in frame popn (not including
  respondent)

- `sibship.size` - total size of reported sibship (including respondent)

## Examples

``` r
  # TODO - add example
```
