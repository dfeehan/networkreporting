# Put estimates from several ties side by side

The simplest and least committal of the three combinations: line the
ties up on the same cells and look at them. Nothing is averaged and
nothing is assumed — which is why it is worth doing first, and often
worth doing instead.

## Usage

``` r
compare_ties(..., estimator = c("ind", "agg"))
```

## Arguments

- ...:

  estimator results, named by tie:
  `compare_ties(siblings = a, cousins = b)`

- estimator:

  `"ind"` (individual visibility) or `"agg"` (aggregate)

## Value

a `tie_comparison`: a tibble with one row per cell per tie, carrying
each tie's estimate, interval and visibility provenance

## Details

Where two ties disagree by more than their intervals allow, that is
information: the ties differ in who they reach, in how completely they
are reported, or in how well their visibility rule holds. Averaging that
away before looking at it would be a mistake.

## See also

[`pool_ties()`](http://dennisfeehan.org/networkreporting/reference/pool_ties.md)
to average them,
[`ties_union_check()`](http://dennisfeehan.org/networkreporting/reference/ties_union_check.md)
for the third combination

## Examples

``` r
if (FALSE) { # \dontrun{
compare_ties(siblings = sib_est, cousins = cousin_est)
} # }
```
