# Visibility predicted from a fitted model

The third member of the predict-from-data family.
[`vis_from_donor()`](http://dennisfeehan.org/networkreporting/reference/vis_from_donor.md)
fits a grouped weighted mean; this fits a model, and is otherwise the
same idea: learn how big a reporting group tends to be from donors whose
group size is known, then predict it for alters whose is not.

## Usage

``` r
vis_from_model(
  formula,
  predictors = NULL,
  family = stats::gaussian(),
  engine = stats::glm,
  donor = "egos",
  on_missing = c("error", "na"),
  label = NULL
)
```

## Arguments

- formula:

  one-sided formula giving the predictors, in the alter's column names

- predictors:

  optional named vector mapping alter column names to donor column
  names, for predictors the two frames spell differently

- family:

  a `family` for `engine`. Defaults to
  [`gaussian()`](https://rdrr.io/r/stats/family.html); a log link such
  as `poisson(link = "log")` is often the better choice, since it cannot
  predict a non-positive group size

- engine:

  the fitting function, taking `formula`, `data`, `family` and
  `weights`. Defaults to
  [`stats::glm()`](https://rdrr.io/r/stats/glm.html)

- donor:

  `"egos"` to fit on the survey respondents, or a data frame

- on_missing:

  what to do about an alter the model cannot predict for: `"error"` or
  `"na"` (leave unresolved, so
  [`vis_coalesce()`](http://dennisfeehan.org/networkreporting/reference/vis_coalesce.md)
  can try the next tier)

- label:

  optional short name for provenance

## Value

a
[visibility_rule](http://dennisfeehan.org/networkreporting/reference/visibility_rule.md)

## Details

A cell mean is a model with one categorical predictor and no pooling.
This buys three things over that: continuous predictors, several
covariates without the cell count collapsing, and borrowing strength
across cells rather than treating each in isolation — which is what
makes it usable where `min_donors` would otherwise empty a cell.

## The formula is one-sided, and speaks the alter's vocabulary

Pass predictors only — `~ age + sex`, not `S ~ age + sex`. The response
is always the donor's own group size, which the rule computes; naming it
would mean knowing an internal.

Write the predictors as the **alter** rows spell them, since that is
where the model has to predict. Where the donor frame spells one
differently, say so with `predictors`, exactly as
[`vis_from_donor()`](http://dennisfeehan.org/networkreporting/reference/vis_from_donor.md)'s
`match_on` does: `predictors = c(.sib.sex = "sex")` reads "the alter
column `.sib.sex` is the donor column `sex`". The donor frame is renamed
before fitting, so the fitted object speaks one vocabulary throughout.

## Bootstrapping this is expensive, and has to be

A model with continuous predictors is not constant within an estimation
cell, so the cheap per-cell identity does not apply to it. The estimator
detects that and refits the model inside every bootstrap replicate, at
roughly M times the cost of a point estimate, warning as it goes. That
is the only correct route: holding a fitted model fixed across
replicates would treat an estimated quantity as known and understate the
variance.

## What it does not check

That the model is any good. The package will tell you if a prediction is
impossible — a non-positive group size — but not if it is merely wrong.
Donors are alive and on the frame while many alters needing an imputed
visibility are dead, so a model fitted on donors extrapolates to a
population it never saw; that assumption is recorded in the provenance
rather than left implied.

## See also

[`vis_from_donor()`](http://dennisfeehan.org/networkreporting/reference/vis_from_donor.md),
the same idea with a cell mean in place of a model

## Examples

``` r
vis_from_model(~ .sib.sex, predictors = c(.sib.sex = "sex"))
#> <visibility_rule: model(.sib.sex)>
#>   requires:     .sib.in.F, .sib.sex
#>   is_estimated: TRUE  (refit within each bootstrap replicate)
#>   parameters:
#>     formula = ~.sib.sex
#>     predictors = .sib.sex
#>     family = gaussian
#>     donor = egos
#>     on_missing = error
#>     label = NULL
#>   assumptions:
#>     - visibility is predicted by a fitted model, ~.sib.sex, rather than derived
#>     - fitted on the survey respondents
#>     - the model is fitted on donors, who are alive and on the frame, and then extrapolated to alters who are frequently neither; the package checks that a prediction is possible, not that it is right
```
