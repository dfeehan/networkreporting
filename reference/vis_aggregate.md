# Visibility from aggregate relational data

Uses respondents' estimated personal network sizes — the output of the
known-population scale-up method — as the basis for visibility. It is
the bridge between this package's two halves: the ARD / scale-up side
estimates how many people a respondent knows, and the estimator spine
needs to know how many people could have reported an alter.

## Usage

``` r
vis_aggregate(
  degree.var,
  frame.ratio = NULL,
  degree.counts = c("population", "frame"),
  donor = "egos",
  statistic = c("harmonic", "arithmetic", "median"),
  match_on = NULL,
  label = NULL
)
```

## Arguments

- degree.var:

  name of the column in the donor frame holding each donor's estimated
  degree, as from
  [`kp.degree.estimator()`](http://dennisfeehan.org/networkreporting/reference/kp.degree.estimator.md)

- frame.ratio:

  the share of the population that is in the frame population,
  `N_F / N`. Required when `degree.counts = "population"`

- degree.counts:

  does `degree.var` count connections to the whole population
  (`"population"`, the default, and what
  [`kp.degree.estimator()`](http://dennisfeehan.org/networkreporting/reference/kp.degree.estimator.md)
  returns) or only to frame members (`"frame"`, where no conversion is
  needed)?

- donor:

  `"egos"` to use the survey respondents, or a data frame

- statistic:

  how to summarise donors' degrees: `"harmonic"` (the default, for the
  reason given in
  [`vis_from_donor()`](http://dennisfeehan.org/networkreporting/reference/vis_from_donor.md)),
  `"arithmetic"` or `"median"`

- match_on:

  optional covariates to match alters to donors on, as in
  [`vis_from_donor()`](http://dennisfeehan.org/networkreporting/reference/vis_from_donor.md)

- label:

  optional short name for provenance

## Value

a
[visibility_rule](http://dennisfeehan.org/networkreporting/reference/visibility_rule.md)

## What it assumes, and why one argument has no default

[`kp.degree.estimator()`](http://dennisfeehan.org/networkreporting/reference/kp.degree.estimator.md)
returns each respondent's degree with respect to the **whole
population**: how many people they know, full stop. Visibility is a
narrower thing — how many *frame-population members* could report an
alter. Converting one into the other needs the share of the population
that is in the frame, and nothing in the data supplies it. That is
`frame.ratio`, and it deliberately has no default, for the same reason
[`tie_config()`](http://dennisfeehan.org/networkreporting/reference/tie_config.md)
has no default structure: getting it wrong scales every estimate by a
constant and nothing complains.

Two further assumptions come with the method rather than with this
function, and are recorded in the provenance:

- the tie is roughly symmetric, so that an alter's connections to frame
  members can be inferred from respondents' connections in general;

- respondents' degrees stand in for alters'. Where the two populations
  differ — and for mortality they differ in the most relevant way, since
  the alters include the dead — this is the same substitution
  [`vis_from_donor()`](http://dennisfeehan.org/networkreporting/reference/vis_from_donor.md)
  makes, with the same direction of error.

## Why the frame split still matters here

This assigns what is essentially one number per matched cell, and a
visibility constant within a cell cancels out of a rate. Preserving the
on-frame / off-frame asymmetry is therefore not a refinement: without it
this rule reduces exactly to the aggregate estimator, and the ARD does
no work at all.

## See also

[`kp.degree.estimator()`](http://dennisfeehan.org/networkreporting/reference/kp.degree.estimator.md),
[`vis_from_donor()`](http://dennisfeehan.org/networkreporting/reference/vis_from_donor.md)

## Examples

``` r
# respondents know ~250 people; a fifth of the population is in the frame
vis_aggregate("d.hat", frame.ratio = 0.2)
#> <visibility_rule: ard(d.hat)>
#>   requires:     .sib.in.F
#>   is_estimated: TRUE  (refit within each bootstrap replicate)
#>   parameters:
#>     degree.var = d.hat
#>     frame.ratio = 0.2
#>     degree.counts = population
#>     statistic = harmonic
#>     match_on = NULL
#>     label = NULL
#>   assumptions:
#>     - visibility comes from respondents' estimated degrees in 'd.hat', summarised by their weighted harmonic mean
#>     - degrees count connections to the whole population and are scaled by frame.ratio = 0.2 to get frame-member connections
#>     - the tie is roughly symmetric, so alters' connections to frame members can be inferred from respondents' connections in general
#>     - respondents' degrees stand in for alters'; for mortality the alters include the dead, so this substitution errs in the same direction as any donor rule
```
