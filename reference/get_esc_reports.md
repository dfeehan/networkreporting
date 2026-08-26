# Get ego X sibling X cell reports

Produces a long-form dataset that has a row for each respondent-sibling
X cell combination

## Usage

``` r
get_esc_reports(sib.dat, sib.id, ego.id, cell.config)
```

## Arguments

- sib.dat:

  The long-form sibling history dataset (likely produced by TODO)

- sib.id:

  String with the name of the column of `sib.dat` that has the sibling
  ID

- ego.id:

  String with the name of the column of `sib.dat` that has the ID of the
  survey respondent

- cell.config:

  An object containing the configuration of cells; see TODO for more
  information

## Value

A tbl with a row for each respondent-sibling X cell

## Examples

``` r
  # TODO
```
