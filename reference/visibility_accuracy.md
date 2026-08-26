# Score a visibility rule against a known truth

Compares predicted visibilities to true ones, **separately for alters in
and out of the frame population**, and reports the ratio between those
two errors.

## Usage

``` r
visibility_accuracy(predicted, truth, in.frame)
```

## Arguments

- predicted:

  numeric vector of predicted visibilities, one per report

- truth:

  numeric vector of true visibilities, the same length

- in.frame:

  logical or 0/1 vector saying whether each **alter** is in the frame
  population. Note this is the alter's status — unlike
  [`true_visibility_from_network()`](http://dennisfeehan.org/networkreporting/reference/true_visibility_from_network.md),
  where it is the reporter's

## Value

a `visibility_accuracy` object: per-side counts, the share predicted
exactly, the mean ratio of predicted to true, and the differential

## Why the split, and why the ratio

Visibility reaches a death rate only through the asymmetry between
on-frame and off-frame alters: every death is off-frame, while exposure
is a mixture. An error of the same size on both sides therefore largely
cancels out of the rate. A *differential* one does not — it biases it.

So an overall accuracy figure is close to useless here. A rule can be
badly wrong on both sides and still give an almost unbiased rate, or
mildly wrong in a lopsided way and bias it substantially. `differential`
is the number to read.

## See also

[`true_visibility_from_network()`](http://dennisfeehan.org/networkreporting/reference/true_visibility_from_network.md)

## Examples

``` r
visibility_accuracy(predicted = c(3, 4, 3, 4),
                    truth     = c(3, 3, 3, 4),
                    in.frame  = c(TRUE, FALSE, TRUE, FALSE))
#> <visibility_accuracy>
#>   scored: 4 report(s)
#> 
#>   side              n    exact    pred/true
#>   off-frame         2    50.0%        1.167
#>   on-frame          2   100.0%        1.000
#> 
#>   differential (off/on) = 1.167  -- BIASES a rate
#>   Every death is off-frame while exposure is a mixture, so it is the
#>   ratio between the two sides, not either one, that reaches the rate.
```
