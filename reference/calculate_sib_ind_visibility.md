# given a sib dataset, calculate individual visibility weight for each sib

given a sib dataset, calculate individual visibility weight for each sib

## Usage

``` r
calculate_sib_ind_visibility(
  df,
  sib.frame.indicator = ".sib.in.F",
  num.sibs.on.frame.var = "y.F",
  varname = "ind_vis"
)
```

## Arguments

- df:

  Dataset on sibling reports (possibly by cell)

- sib.frame.indicator:

  String with the name of the column in `df` containing a 0/1 coded
  variable indicating whether or not each sib is in the frame population

- num.sibs.on.frame.var:

  String with the name of the column in `df` containing the number of
  sibs on the frame for each ego

- varname:

  String with the name of the new column to create with the individual
  visibility of each sib

## Value

`df` with a new column that has the individual visibility of each
sibling
