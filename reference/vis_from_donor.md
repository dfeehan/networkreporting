# Visibility borrowed from a donor population

The approximation, and the first `is_estimated = TRUE` rule. Where
visibility cannot be derived from ego's own reports – which is the
normal case for any tie that is not a clique – borrow it from a donor
population, optionally matched on covariates.

## Usage

``` r
vis_from_donor(
  donor = "egos",
  match_on = NULL,
  statistic = c("harmonic", "arithmetic", "median"),
  donor_vis = vis_from_clique(),
  min_donors = 25,
  on_missing = c("error", "fallback", "na")
)
```

## Arguments

- donor:

  `"egos"` to use the survey respondents, or a data frame of donors
  supplied directly

- match_on:

  covariates to match alters to donors on, or `NULL` for one global
  value. Names the alter's columns; where the donor frame spells a
  covariate differently, use a named vector, as in `c(.sib.sex = "sex")`

- statistic:

  `"harmonic"` (the default, and the right one for the individual
  estimator), `"arithmetic"` or `"median"`

- donor_vis:

  how the donors' *own* visibility is derived; a
  [visibility_rule](http://dennisfeehan.org/networkreporting/reference/visibility_rule.md),
  defaulting to
  [`vis_from_clique()`](http://dennisfeehan.org/networkreporting/reference/vis_from_clique.md)

- min_donors:

  cells with fewer donors than this are treated as having no donors at
  all

- on_missing:

  what to do about an alter whose donor cell is missing or too small:
  `"error"`, `"fallback"` (use the global value) or `"na"`

## Value

a
[visibility_rule](http://dennisfeehan.org/networkreporting/reference/visibility_rule.md)

## Details

Read it as the simplest member of the predict-from-data family: `fit()`
is a grouped weighted mean where a model's would be a regression.

## What this assumes, and which way it is wrong

Donors are respondents: alive, and on the frame. A large share of the
alters needing an imputed visibility are dead. Wherever visibility
correlates with mortality – through family size for kin ties, through
living arrangements for household ties – the donor is systematically
wrong, not merely noisy. The direction is recorded in the rule's
assumptions so that it reaches the provenance table rather than staying
in a methods appendix.

## Harmonic or arithmetic

The default is the weighted harmonic mean, because the individual
estimator averages `1/v`: the functional that makes the plug-in unbiased
is `(E[1/v])^-1`, not `E[v]`. `"arithmetic"` remains available, since it
is what the historical `y.F.bar / (y.F.bar + 1)` adjustment factor used.
By Jensen's inequality harmonic \<= arithmetic, so the two disagree in a
known direction, by an amount that grows with the variance of
visibility.

## Coverage failure is routine

`match_on` describes the alter, but donors are respondents. DHS
interviews women aged 15-49, so an alter aged 60 has no donor cell at
all. Left alone that surfaces as `NA` propagating silently into rates;
`min_donors` together with `on_missing = "error"` makes it loud instead.
Use
[`vis_coalesce()`](http://dennisfeehan.org/networkreporting/reference/vis_coalesce.md)
to fall back to a coarser rule rather than to `NA`.

## Examples

``` r
vis_from_donor(match_on = c(.sib.sex = "sex"))
#> <visibility_rule: donor(.sib.sex)>
#>   requires:     .sib.in.F, .sib.sex
#>   is_estimated: TRUE  (refit within each bootstrap replicate)
#>   parameters:
#>     donor = egos
#>     match_on = .sib.sex
#>     statistic = harmonic
#>     donor_vis = <clique>
#>     min_donors = 25
#>     on_missing = error
#>   assumptions:
#>     - visibility is borrowed from the survey respondents, matched on .sib.sex
#>     - donor visibilities are summarised by their weighted harmonic mean
#>     - donors are alive and on the frame, but many alters needing an imputed visibility are dead; where visibility correlates with mortality the donor is systematically wrong, not merely noisy
```
