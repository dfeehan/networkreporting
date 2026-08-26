# Read a population total off an estimate

A death rate is a ratio of two visibility-adjusted sums. Those sums are
estimands in their own right, and this reads one of them out with its
uncertainty.

## Usage

``` r
estimated_total(res, of = c("events", "exposure"), estimator = c("ind", "agg"))
```

## Arguments

- res:

  a result from
  [`network_survival_estimator()`](http://dennisfeehan.org/networkreporting/reference/network_survival_estimator.md)

- of:

  `"events"` for the numerator — deaths, for a mortality estimate — or
  `"exposure"` for the denominator, the estimated person-time

- estimator:

  `"ind"` (individual visibility) or `"agg"` (aggregate)

## Value

a tibble with one row per cell: the estimate, and its interval and
standard error where the estimate was bootstrapped

## Why this is a separate step

[`network_survival_estimator()`](http://dennisfeehan.org/networkreporting/reference/network_survival_estimator.md)
computes visibility-adjusted weighted sums and then divides one by the
other. The division is the last thing it does, and nothing before it
assumes a ratio is what you want — the visibility layer in particular
has no idea what is being summed. So a total needs no new estimation,
only a different final step.

## What makes it a population total

The sum is only an estimate of a *population* total if the survey
weights are population weights. With relative or normalised weights it
is a total on whatever scale those weights carry, and the number will be
wrong by a constant factor — silently, since nothing in the data says
which kind of weight was used. The rate is unaffected either way,
because the factor cancels; that is exactly why this distinction can go
unnoticed until somebody asks for a total.

## See also

[`network_survival_estimator()`](http://dennisfeehan.org/networkreporting/reference/network_survival_estimator.md)

## Examples

``` r
if (FALSE) { # \dontrun{
estimated_total(res, "events")     # estimated deaths in the population
estimated_total(res, "exposure")   # estimated person-time
} # }
```
