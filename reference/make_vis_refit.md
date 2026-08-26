# Build a per-replicate refit function for an estimated visibility rule

For a rule with `is_estimated = TRUE`, the estimated group size moves
with the bootstrap replicate. This returns the `refit` closure
[`get_boot_ests_matrix()`](http://dennisfeehan.org/networkreporting/reference/get_boot_ests_matrix.md)
expects: given a replicate index, it refits the rule using that
replicate's weights and returns the group size `S.hat` for each row of
`ec.dat`.

## Usage

``` r
make_vis_refit(rule, donor.dat, boot.weights, ec.dat, ego.id = ".ego.id")
```

## Arguments

- rule:

  the
  [visibility_rule](http://dennisfeehan.org/networkreporting/reference/visibility_rule.md)

- donor.dat:

  the donor frame

- boot.weights:

  data frame of bootstrap weights, with an ego id column and columns
  `boot_weight_1` ... `boot_weight_M`

- ec.dat:

  the ego X cell data the estimate is computed from

- ego.id:

  name of the ego id column

## Value

`function(r)` returning a numeric vector, one `S.hat` per row of
`ec.dat`, or `NULL` if the rule does not estimate from the sample

## Details

Predicting with the frame indicator set to zero returns `S.hat` itself,
since a rule's visibility for an off-frame alter *is* the group size.
