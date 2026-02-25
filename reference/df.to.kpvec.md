# turn a dataframe into a known population vector

`df.to.kpvec` takes a dataframe which has a column with known population
names, and a column with known population totals, and turns it into a
known population vector. if the names of the survey variables
corresponding to each known population are available, they can be passed
in as well

## Usage

``` r
df.to.kpvec(kp.data, kp.var, kp.value)
```

## Arguments

- kp.data:

  the known population dataset

- kp.var:

  the column of `kp.data` that has known population names; either a
  column name, a column index, or a vector of values

- kp.value:

  the column of `kp.data` that has known population sizes; either a
  column name, a column index, or a vector of value

## Value

a vector whose entries have the known population values and whose names
have the corresponding `kp.var` value

## See also

[add.kp](http://dennisfeehan.org/networkreporting/reference/add.kp.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  ## see example in add.kp
} # }
```
