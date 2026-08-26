# helper function for calculating aggregate visibility estimate from ego X cell data

helper function for calculating aggregate visibility estimate from ego X
cell data

## Usage

``` r
get_agg_est_from_ec(ec_dat, wgt_var, cell_vars)
```

## Arguments

- ec_dat:

  the ego X cell data

- wgt_var:

  either a string with the name of the column that has sampling weights
  or a vector with the names of columns with bootstrap weights

- cell_vars:

  vector of strings with the names of variables to group by (the cells)

## Value

a tibble with the individual visibility ASDR estimates (not including
the respondents' exposures)
