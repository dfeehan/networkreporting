# Count group members on the frame, for each ego

The generic form of
[`get_sibship_info()`](http://dennisfeehan.org/networkreporting/reference/get_sibship_info.md).
Counts, for each ego, how many of the alters ego reports are in the
frame population, and adds one for ego when ego is a member of the group
ego reports about.

## Usage

``` r
get_group_info(dat, ego.id, frame.indicator, ego.in.group = TRUE)
```

## Arguments

- dat:

  long-form alter data, one row per ego X alter

- ego.id:

  name of the column holding the ego id

- frame.indicator:

  name of the 0/1 column saying whether each alter is in the frame
  population

- ego.in.group:

  is ego a member of the group being reported about?

## Value

a tibble with one row per ego and columns `y.F` (alters on frame, not
counting ego), `yprime.F` (on-frame group members including ego when
`ego.in.group`) and `sib.size` (group size)

## Details

That `+ 1` is the whole content of `ego.in.group`. It is not about
siblings: it encodes "ego belongs to the group ego reports about", which
is true of siblings and of households, and false of parents and of
neighbours.
