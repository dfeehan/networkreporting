# Weighted harmonic mean

The individual estimator averages `1/v`, so the functional that makes a
plug-in visibility unbiased is `(E[1/v])^-1`, not `E[v]`. This is the
default summary for
[`vis_from_donor()`](http://dennisfeehan.org/networkreporting/reference/vis_from_donor.md).

## Usage

``` r
whmean(x, w)
```

## Arguments

- x:

  numeric vector; must be strictly positive

- w:

  numeric vector of weights, the same length as `x`

## Value

the weighted harmonic mean, `sum(w) / sum(w/x)`

## Details

Restored from
[`get_visibility()`](http://dennisfeehan.org/networkreporting/reference/get_visibility.md),
which carried it as `wh.mean()` until the ad-hoc adjustment factors were
removed.
