# Approximating visibility

``` r

library(networkreporting)
library(dplyr)
library(tibble)
```

## What visibility is, and why it usually cannot be calculated

A network reporting estimator divides each report by the number of
frame-population members who could have made it. That number is the
alter’s **visibility**, and it is what converts “how many reports did we
see” into “how big is the population”.

For siblings, visibility is not a modelling choice. It can be read
straight off ego’s own roster:

- an alter who is **on the frame** has visibility `y.F`
- an alter who is **not** has visibility `y.F + 1`

where `y.F` is the number of ego’s siblings who are in the frame
population. That is
[`vis_from_clique()`](http://dennisfeehan.org/networkreporting/reference/vis_from_clique.md),
and it is the default everywhere.

``` r

vis_from_clique()
#> <visibility_rule: clique>
#>   requires:     y.F, .sib.in.F
#>   is_estimated: FALSE  (fit once; frozen across bootstrap replicates)
#>   parameters:
#>     ego.in.group = TRUE
#>   assumptions:
#>     - the tie partitions the population into disjoint groups
#>     - ego is a member of the group ego reports about
#>     - reporting within the group is complete
```

The three assumptions it prints are the whole reason it works.
Siblingship is an equivalence relation, so it cuts the population into
disjoint groups; ego belongs to the group ego reports about; and
reporting within the group is complete. Given those three, ego’s roster
is enough to recover the visibility of *every* alter, including alters
whose own neighbourhoods ego cannot see.

Households satisfy all three. Most other ties do not:

| Tie        | Partitions?             | Ego in group? | Verdict              |
|------------|-------------------------|---------------|----------------------|
| siblings   | yes                     | yes           | exact                |
| household  | yes                     | yes           | exact                |
| cousins    | **no** (not transitive) | yes           | not identified       |
| parents    | yes                     | **no**        | needs another roster |
| neighbours | **no**                  | yes           | not identified       |

For those, visibility generally is not identified from one-sided reports
at all. The standard move is to substitute a summary of the
*respondents’* visibility. That is a real assumption with a knowable
bias, and the point of this machinery is to give it somewhere to live
other than an undocumented `case_when` in analysis code.

## The subtlety that determines the whole design

It is tempting to think that a visibility which is constant within an
estimation cell cancels out of a rate — the rate is a ratio, and both
numerator and denominator get divided by the same number.

**It does not**, and seeing why is what makes the rest of this make
sense.

Deaths and exposure do not receive the same visibility:

- a dead alter is never on the frame, so **every death** gets
  `1/(y.F + 1)`
- exposure is a **mixture**: living on-frame alters get `1/y.F`, living
  off-frame alters get `1/(y.F + 1)`

So visibility survives into the rate purely through that asymmetry. Here
is the consequence, made explicit:

``` r

alters <- tibble(
  .ego.id   = c(1, 1, 1, 2, 2),
  .sib.in.F = c(1,  1,  0,  1,  0),
  y.F       = c(2,  2,  2,  1,  1),
  sib.occ   = c(0,  0,  1,  0,  1),   # deaths, only ever off-frame
  sib.exp   = c(10, 10, 4,  10, 6)    # exposure, a mixture
)

rate <- function(w) sum(alters$sib.occ * w) / sum(alters$sib.exp * w)

# a rule that ignores frame status: one number for every alter
flat <- rep(1/4, nrow(alters))

# the clique rule, which preserves the split
clique <- vis_from_clique()
split  <- clique$predict(alters, clique$fit(NULL, NULL))$vis_weight

c(flat      = rate(flat),
  aggregate = sum(alters$sib.occ) / sum(alters$sib.exp),
  split     = rate(split))
#>       flat  aggregate      split 
#> 0.05000000 0.05000000 0.03424658
```

The flat rule reproduces the **aggregate** estimator exactly, because a
constant divides out of a ratio. An approximation that assigns one
number per cell without keeping the on-frame/off-frame split therefore
does nothing at all.

This is why
[`vis_from_donor()`](http://dennisfeehan.org/networkreporting/reference/vis_from_donor.md)
estimates a *group size* and then reconstructs the split from it, rather
than estimating a visibility directly.

## Borrowing visibility from donors

[`vis_from_donor()`](http://dennisfeehan.org/networkreporting/reference/vis_from_donor.md)
takes a donor population — normally the survey respondents, whose own
visibility *is* derivable — summarises it, and applies the result to
alters that cannot be resolved exactly.

``` r

rule <- vis_from_donor(match_on = c(.sib.sex = "sex"), min_donors = 10)
rule
#> <visibility_rule: donor(.sib.sex)>
#>   requires:     .sib.in.F, .sib.sex
#>   is_estimated: TRUE  (refit within each bootstrap replicate)
#>   parameters:
#>     donor = egos
#>     match_on = .sib.sex
#>     statistic = harmonic
#>     donor_vis = <clique>
#>     min_donors = 10
#>     on_missing = error
#>   assumptions:
#>     - visibility is borrowed from the survey respondents, matched on .sib.sex
#>     - donor visibilities are summarised by their weighted harmonic mean
#>     - donors are alive and on the frame, but many alters needing an imputed visibility are dead; where visibility correlates with mortality the donor is systematically wrong, not merely noisy
```

Two things to notice in that printout.

`is_estimated` is `TRUE`. The estimated group size is a **sample**
quantity, so holding it fixed across bootstrap replicates would
understate the variance. `is_estimated` is what drives the replicate
loop to refit rather than freeze; for
[`vis_from_clique()`](http://dennisfeehan.org/networkreporting/reference/vis_from_clique.md)
it is `FALSE`, and freezing is then correct, because visibility is a
function of ego’s own reports rather than of who was sampled.

And the assumptions include the direction of the bias, not just its
existence. Donors are alive and on the frame. Many of the alters needing
an imputed visibility are dead. Wherever visibility correlates with
mortality — through family size for kin ties, living arrangements for
household ties — the donor is systematically wrong, not merely noisy.

### Harmonic, not arithmetic

The default summary is the weighted **harmonic** mean, and that is not a
detail. The individual estimator averages `1/v`, so the functional that
makes the plug-in unbiased is `(E[1/v])^-1`, not `E[v]`.

``` r

# four donors with very different sibship sizes
spread <- tibble(.ego.id = 1:4, y.F = c(1, 2, 3, 11), w = 1)
one    <- tibble(.ego.id = 1, .sib.in.F = 0)

S_under <- function(stat) {
  r <- vis_from_donor(match_on = NULL, statistic = stat, min_donors = 1)
  r$predict(one, r$fit(spread, "w"))$vis
}

c(harmonic   = S_under("harmonic"),
  arithmetic = S_under("arithmetic"))
#>   harmonic arithmetic 
#>   3.428571   5.250000
```

By Jensen’s inequality harmonic `<=` arithmetic always, so the two
disagree in a *known direction*, by an amount that grows with the
variance of visibility. They coincide exactly when there is no variance
at all.

`"arithmetic"` remains available, because it is what the historical
`y.F.bar / (y.F.bar + 1)` adjustment factor used. If you are reproducing
an older analysis, that is the setting that will match it.

## Donor coverage fails, routinely

`match_on` describes the **alter**. Donors are **respondents**. DHS and
MICS interview women aged 15–49, so an alter aged 60, or any male alter,
has no donor cell at all.

Left alone, that produces `NA` and the `NA` propagates silently into
rates. `min_donors` together with `on_missing` makes it loud instead:

``` r

donors <- tibble(.ego.id = 1:40, y.F = 3, sex = "f", w = 1)
alters <- tibble(.ego.id = c(1, 2), .sib.in.F = c(0, 0), .sib.sex = c("f", "m"))

strict <- vis_from_donor(match_on = c(.sib.sex = "sex"), min_donors = 10)
strict$predict(alters, strict$fit(donors, "w"))
#> Error in `strict$predict()`:
#> ! 1 of 2 alter row(s) have no usable donor cell (missing, or fewer than 10 donors).
#> This is routine rather than exceptional: match_on describes the alter, but donors are respondents, and a survey of women aged 15-49 has no donor for an alter aged 60.
#> Use on_missing = 'fallback' to take the global value instead, or wrap this rule in vis_coalesce() to fall through to a coarser one. Unresolved cells:
#> # A tibble: 1 × 1
#>   .sib.sex
#>   <chr>   
#> 1 m
```

The three options are `"error"` (the default, above), `"fallback"` (use
the global value and say so), and `"na"` (leave it unresolved, so that a
coalesced rule can try the next tier).

## You must say what the tie is

[`vis_from_clique()`](http://dennisfeehan.org/networkreporting/reference/vis_from_clique.md)
will not run until you declare what kind of tie the reports are about:

``` r

apply_visibility_rule(vis_from_clique(), esc.dat = tibble(
  .ego.id = 1, .sib.in.F = 0, y.F = 2))
#> Error in `apply_visibility_rule()`:
#> ! visibility rule 'clique' is only valid for tie structure(s): clique,
#> and no tie was declared. Pass tie = tie_config("...").
#> 
#> This is deliberate. Applicability cannot be read off the data: on a tie that is not a clique, vis_from_clique() still returns a finite, plausible number, and it is wrong. Siblings and household members are 'clique'; cousins are 'group'; parents are 'star'; neighbours and acquaintances are 'unbounded'.
```

This is deliberate, and it is the most important safety property in this
vignette. The clique rule returns a finite, plausible number for *any*
roster. Nothing in the data distinguishes a tie that satisfies its
assumptions from one that does not, so applied in the wrong place it is
wrong with no outward sign – and the provenance table then reports
`clique: 100%`, which reads as “the exact rule was used”.

How wrong? Socsim gives the true reporting network, and therefore each
alter’s true visibility, so this is measurable rather than a matter of
opinion:

| roster | off-frame (carries the deaths) | on-frame (exposure only) | differential |
|----|----|----|----|
| maternal siblings | exact (100%) | exact (100%) | 1.000 |
| maternal cousins | exact (100%) | exact (100%) | 1.000 |
| paternal cousins | exact (100%) | exact (100%) | 1.000 |
| maternal **union** paternal cousins | 1.089x too high (65% exact) | 1.061x too high (63% exact) | **1.026** |

The interesting row is the last one, and the interesting non-rows are
the two above it.

“Cousinship is not transitive” is the usual reason given for cousins not
being a clique, and in general it is true: your maternal cousin and your
paternal cousin are not each other’s cousins. But *within one line* it
is transitive — everyone sharing a maternal grandmother forms an
equivalence class — so a maternal-cousin roster is a clique, and the
rule is exact on it. What breaks is pooling the two lines.

So the lesson is not that cousins are a special case to be careful of.
It is that whether a given roster is a clique is a question about **how
that roster was built**, which no amount of looking at the data will
answer. Hence the declaration.

Note *where* the pooled roster goes wrong: more on the off-frame side,
which carries every death, than on the on-frame side. By the argument
above, a uniform error would cancel out of the rate; this differential
one does not.

So declare the tie:

``` r

tie_config("clique", name = "siblings")
#> <tie_config>
#>   structure:       clique
#>   name:            siblings
#>   ego.in.group:    (not declared; the rule's own setting is used)
#>   frame.indicator: (not declared)
tie_config("group",  name = "maternal cousins")
#> <tie_config>
#>   structure:       group
#>   name:            maternal cousins
#>   ego.in.group:    (not declared; the rule's own setting is used)
#>   frame.indicator: (not declared)
```

`"clique"` is siblings and household members; `"group"` is cousins;
`"star"` is parents, whose visibility is a fact about the *sibship*
rather than the parent roster; `"unbounded"` is neighbours and
acquaintances. A rule that makes no structural assumption, such as
[`vis_from_donor()`](http://dennisfeehan.org/networkreporting/reference/vis_from_donor.md),
needs no declaration and accepts any.

### What else belongs on the tie

Two further things are facts about the tie rather than about the rule,
and a
[`tie_config()`](http://dennisfeehan.org/networkreporting/reference/tie_config.md)
can carry both:

``` r

tie_config("star", name = "parents",
           ego.in.group    = FALSE,     # ego is not in the group being sized
           frame.indicator = "in.frame") # this tie's own eligibility column
#> <tie_config>
#>   structure:       star
#>   name:            parents
#>   ego.in.group:    FALSE
#>   frame.indicator: in.frame
```

`ego.in.group` is the `+ 1`: it says whether ego is counted among the
group members who could report an alter. `frame.indicator` names the
column saying who is in the frame population *for this tie*, which
starts to matter once ties differ in who is eligible — neighbours
bounded by a compound, say, against siblings who are not.

Both default to undeclared, in which case nothing changes: the rule’s
own setting is used, and the frame column defaults to `.sib.in.F`.

Where a tie declares one of these and the rule or the call also sets it,
**agreement is fine and disagreement is an error naming both sources**:

``` r

apply_visibility_rule(
  vis_from_clique(ego.in.group = TRUE),
  esc.dat = tibble(.ego.id = 1, .sib.in.F = 0, y.F = 2),
  tie     = tie_config("clique", name = "siblings", ego.in.group = FALSE))
#> Error in `reconcile_tie_setting()`:
#> ! conflicting values for 'ego.in.group'.
#>   tie_config() declares: FALSE
#>   visibility rule 'clique' sets: TRUE
#> 
#> These disagree, and neither silently wins: 'ego.in.group' is a property of the tie, so a rule set against it would compute under an assumption you did not choose. Set it in one place, or set both to the same value.
```

There is deliberately no rule for which one wins. Silent precedence
would compute a number under an assumption you did not choose — the same
failure the tie gate itself exists to prevent — so the package makes you
say it once.

Note that what a tie declares reaches the provenance table, including
the assumptions, which are recomputed from the setting actually used
rather than from whatever the rule was built with:

``` r

apply_visibility_rule(
  vis_from_clique(),
  esc.dat = tibble(.ego.id = c(1, 1), .sib.in.F = c(1, 0), y.F = c(2, 2)),
  tie     = tie_config("clique", name = "households",
                       ego.in.group = FALSE))$provenance
#> <vis_provenance>
#>   rule:         clique
#>   is_estimated: FALSE
#>   tie:          clique (households)
#>   ego.in.group: FALSE
#>   alters:       2
#>   resolved by:
#>     clique                              2  (100.0%)
#>   approximated: 0.0% of alters, n/a of deaths, n/a of exposure
#>   assumptions:
#>     - the tie partitions the population into disjoint groups
#>     - ego is NOT a member of the group ego reports about, so ego is not counted
#>     - reporting within the group is complete
```

## When the group is not the roster

[`vis_from_clique()`](http://dennisfeehan.org/networkreporting/reference/vis_from_clique.md)
works out the group by counting ego’s roster. Sometimes the group whose
size sets an alter’s visibility is *not* the roster the alter was
reported on, and only you can say what it is:

- **Pooled ties.** Cousins are reported through the maternal and
  paternal sides separately, but an alter’s visibility depends on
  everyone who could have reported them — both sides, plus ego’s own
  siblings. That group is not any single roster.
- **Nested ties.** “Pooled cousins, excluding ego’s siblings” is a
  difference of two counts. No roster holds it.
- **Borrowed groups.** A parent’s visibility is the number of their
  children on the frame — a fact about the *sibship*, not the parent
  roster.

[`vis_from_group_size()`](http://dennisfeehan.org/networkreporting/reference/vis_from_group_size.md)
takes the count from a column instead of deriving it:

``` r

cousins <- tibble(
  .ego.id   = c(1, 1, 2),
  .sib.in.F = c(1, 0, 1),
  n_pooled_and_F  = c(5, 5, 3),   # ego's pooled cousinship, on frame
  n_sibship_and_F = c(2, 2, 1))
cousins$n_pooled_nosib_and_F <-
  cousins$n_pooled_and_F - cousins$n_sibship_and_F

pooled <- vis_from_group_size("n_pooled_and_F", label = "pooled cousins")
pooled$predict(cousins, pooled$fit(NULL, NULL))$vis
#> [1] 4 5 2
```

The rule still does the frame split — an alter who is themselves in the
frame population cannot report themselves — because that asymmetry is
the only route by which visibility reaches a rate. A basis that
deliberately omits it says so:

``` r

nosib <- vis_from_group_size("n_pooled_nosib_and_F",
                             subtract.self = FALSE,
                             label = "pooled without siblings")
nosib$predict(cousins, nosib$fit(NULL, NULL))$vis
#> [1] 3 3 2
```

Two things to be clear about. This rule makes **no structural
assumption**, so it needs no `tie` and accepts any — but that is not
because it is safe the way
[`vis_from_donor()`](http://dennisfeehan.org/networkreporting/reference/vis_from_donor.md)
is safe. It is because you have taken on the part
[`vis_from_clique()`](http://dennisfeehan.org/networkreporting/reference/vis_from_clique.md)
would have derived. The package checks the arithmetic; it cannot check
that your column counts the right people, and the assumptions say so:

``` r

pooled$assumptions
#> [1] "visibility is taken from the supplied column 'n_pooled_and_F', which the caller has computed; the package does not check that it counts the right people"
#> [2] "that column already counts ego"                                                                                                                          
#> [3] "an alter in the frame population does not count themselves"
```

And `label` earns its place when you are comparing bases rather than
committing to one: run the same cells under each, and the provenance
says which produced which.

## From a cell mean to a model

[`vis_from_donor()`](http://dennisfeehan.org/networkreporting/reference/vis_from_donor.md)
fits a grouped weighted mean.
[`vis_from_model()`](http://dennisfeehan.org/networkreporting/reference/vis_from_model.md)
fits a model. Everything else about them is the same, which is the
point: a cell mean *is* a model, with one categorical predictor and no
pooling.

``` r

set.seed(7)
donors <- tibble(.ego.id = 1:400, age = runif(400, 20, 60), w = 1)
donors$y.F <- rpois(400, lambda = exp(0.2 + 0.02 * donors$age))

alters <- tibble(.ego.id = 1:4, .sib.in.F = c(1, 0, 1, 0),
                 age = c(25, 25, 55, 55))

mod <- vis_from_model(~ age, family = poisson(link = "log"))
mod$predict(alters, mod$fit(donors, "w"))$vis
#> [1] 1.967652 2.967652 3.654004 4.654004
```

Three things a cell mean cannot do, and this can: continuous predictors,
several covariates at once without the cell counts collapsing, and
borrowing strength across cells instead of treating each in isolation —
which is what makes it usable where `min_donors` would empty a cell
entirely.

Note the formula is **one-sided** and written in the *alter’s* column
names. The response is always the donor’s own group size, so naming it
would mean knowing an internal. Where the donor frame spells a predictor
differently, map it the same way `match_on` does:
`predictors = c(.sib.age = "age.cat")`.

### Two things to expect

**Bootstrapping it is expensive.** A model with continuous predictors is
not constant within an estimation cell, so the cheap per-cell shortcut
does not apply. The estimator detects that, warns, and refits the model
inside every replicate — roughly M times the cost of a point estimate.
That is the only correct route: holding a fitted model fixed across
replicates would treat an estimated quantity as known.

**A non-positive prediction is an error, not a small number.** An
identity link extrapolating past its donors can predict a group size
below zero, which would become a negative visibility and then a negative
weight. The package refuses it and points at a log link, which cannot:

``` r

falling <- tibble(.ego.id = 1:60, age = seq(20, 60, length.out = 60), w = 1)
falling$y.F <- pmax(0, round(8 - 0.2 * falling$age))

bad <- vis_from_model(~ age)                # gaussian: identity link
bad$predict(tibble(.ego.id = 1, .sib.in.F = 0, age = 200),
            bad$fit(falling, "w"))
#> Error in `bad$predict()`:
#> ! 1 of 1 predicted group size(s) are zero or negative, which cannot be: every alter here was reported by somebody.
#> This is what an identity link does when it extrapolates. Fit with a log link -- family = poisson(link = 'log') -- which cannot predict a non-positive value.
```

What the package does **not** check is whether the model is any good.
Donors are alive and on the frame; many alters needing an imputed
visibility are neither. A model fitted on donors extrapolates to a
population it never saw, and that assumption is recorded in the
provenance rather than left implied.

## Just asking

Every rule so far recovers visibility from something the survey
collected for another purpose: the tie’s structure, or the respondents’
own group sizes. All of them are attempts to get at a quantity nobody
was asked about.

For an `"unbounded"` tie there is no way round that. Neighbours and
acquaintances have no bounded group to count, so there is no roster to
derive from — and no particular reason to think respondents resemble
their alters. Asking is the only route to the quantity:

``` r

asked <- tibble(
  .ego.id   = 1:4,
  .sib.in.F = c(1, 0, 1, 0),
  # "how many people like you does this person know?"
  n_known   = c(4, 3, 2, 5))

rep_rule <- vis_from_report("n_known")
rep_rule$predict(asked, rep_rule$fit(NULL, NULL))$vis
#> [1] 4 3 2 5
```

This is a **survey-design** choice as much as an analysis one. It costs
questionnaire time and can only be used if somebody decided in advance
to ask. Where the question was asked, though, it beats approximating
from something else — and it puts the uncertainty somewhere visible, in
reporting error, rather than in an assumption.

Two things the wording of the question decides, which the numbers cannot
reveal:

- `counts.ego` — did the respondent count *themselves*? “How many people
  like you does X know” usually includes them; “how many **other**
  people like you” does not, and one has to be added back.
- `counts.self` — did the answer count the alter? A question about a
  *group* (“how many people are in X’s household?”) does; one about
  connections does not. When it does,
  [`vis_from_group_size()`](http://dennisfeehan.org/networkreporting/reference/vis_from_group_size.md)
  is the clearer constructor.

A reported visibility below one is treated as an error rather than a
small number, because it contradicts the report it sits on — the alter
was named by somebody, so at least one frame member could see them. The
usual causes are a don’t-know code stored as `0`, or `counts.ego` set
wrongly:

``` r

bad <- asked
bad$n_known[2] <- 0
rep_rule$predict(bad, rep_rule$fit(NULL, NULL))
#> Error in `rep_rule$predict()`:
#> ! 1 of 4 reported visibilities are below 1.
#> That contradicts the report itself: this alter was named by a respondent, so at least one frame member could see them. Common causes are a don't-know code stored as 0, or counts.ego = TRUE when the question actually excluded the respondent.
#> Use on_impossible = 'floor' to raise them to 1, or 'na' to leave them for another tier.
```

Item non-response on this question is normal, so `on_missing = "na"` and
a fallback tier is usually the right shape rather than stopping:

``` r

partial <- asked
partial$n_known[3] <- NA
partial$y.F <- 2

chain <- vis_coalesce(vis_from_report("n_known", on_missing = "na"),
                      vis_from_clique())
out <- apply_visibility_rule(chain, partial,
                             tie = tie_config("clique", name = "siblings"))
out$values
#> # A tibble: 4 × 4
#>     vis vis_weight vis_rule          vis_tier
#>   <dbl>      <dbl> <chr>                <int>
#> 1     4      0.25  reported(n_known)        1
#> 2     3      0.333 reported(n_known)        1
#> 3     2      0.5   clique                   2
#> 4     5      0.2   reported(n_known)        1
```

## Coalescing: exact where possible, approximate where not

[`vis_coalesce()`](http://dennisfeehan.org/networkreporting/reference/vis_coalesce.md)
takes rules in priority order. For each row the first rule returning a
non-`NA` visibility wins, and the row records which tier resolved it.

``` r

mixed <- tibble(
  .ego.id   = c(1, 2, 3),
  .sib.in.F = c(0, 0, 0),
  .sib.sex  = c("f", "f", "f"),
  y.F       = c(2, NA, NA)     # only ego 1 has an exact roster
)

chain <- vis_coalesce(
  vis_from_clique(),                                    # exact where it exists
  vis_from_donor(match_on = c(.sib.sex = "sex"),        # matched approximation
                 min_donors = 10, on_missing = "na"),
  vis_from_donor(match_on = NULL, min_donors = 1)       # global, last resort
)

out <- chain$predict(mixed, chain$fit(donors, "w"))
#> Warning in rules[[i]]$predict(alter.rows[todo, , drop = FALSE], state[[i]]):
#> restarting interrupted promise evaluation
#> Warning in rules[[i]]$predict(alter.rows[todo, , drop = FALSE], state[[i]]):
#> restarting interrupted promise evaluation
out
#> # A tibble: 3 × 4
#>     vis vis_weight vis_rule vis_tier
#>   <dbl>      <dbl> <chr>       <int>
#> 1    NA         NA NA             NA
#> 2    NA         NA NA             NA
#> 3    NA         NA NA             NA
```

Ego 1 is resolved exactly by tier 1; the others fall through to the
donor tier. `vis_tier` records which one caught each row.

That example falls through because tier 1 had no `y.F` to work with. The
other way a chain falls through is by *declaration*: on a tie the clique
rule does not apply to, the tier is dropped outright.

``` r

res <- apply_visibility_rule(
  vis_coalesce(vis_from_clique(), vis_from_donor(match_on = NULL, min_donors = 1)),
  esc.dat = tibble(.ego.id = c(1, 2), .sib.in.F = c(0, 1), y.F = c(2, 2)),
  ego.dat = donors, weights = "w",
  tie     = tie_config("group", name = "cousins"))

res$provenance$dropped_tiers
#> [1] "clique"
```

This is the distinction worth holding onto: the clique tier is removed
because it is *inapplicable*, not skipped because it returned `NA`. It
never returns `NA` – which is why, before the tie had to be declared, a
chain like this one claimed every cousin alter for the exact tier.

## Reading the provenance table

[`apply_visibility_rule()`](http://dennisfeehan.org/networkreporting/reference/apply_visibility_rule.md)
returns the values alongside a provenance object, and
`sibling_estimator()` attaches that object to its result. It is what
turns “some of this was approximated” into a number.

``` r

res <- apply_visibility_rule(vis_from_clique(), esc.dat = tibble(
  .ego.id   = c(1, 1, 1, 2, 2),
  .sib.in.F = c(1, 1, 0, 1, 0),
  y.F       = c(2, 2, 2, 1, 1),
  sib.occ   = c(0, 0, 1, 0, 1),
  sib.exp   = c(10, 10, 4, 10, 6)
), tie = tie_config("clique", name = "siblings"))

res$provenance
#> <vis_provenance>
#>   rule:         clique
#>   is_estimated: FALSE
#>   tie:          clique (siblings)
#>   ego.in.group: TRUE
#>   alters:       5
#>   resolved by:
#>     clique                              5  (100.0%)
#>   approximated: 0.0% of alters, 0.0% of deaths, 0.0% of exposure
#>   assumptions:
#>     - the tie partitions the population into disjoint groups
#>     - ego is a member of the group ego reports about
#>     - reporting within the group is complete
```

The two share-of lines are separate on purpose. The share of *deaths*
that were approximated and the share of *exposure* that were
approximated are different numbers, and an approximation touching very
little exposure but most of the deaths is not a small approximation.
Reporting only one of them would hide exactly the case worth worrying
about.

## Where this is going

[`vis_from_clique()`](http://dennisfeehan.org/networkreporting/reference/vis_from_clique.md)
and
[`vis_from_donor()`](http://dennisfeehan.org/networkreporting/reference/vis_from_donor.md)
are two implementations of one interface, split into `fit` and
`predict`. That split is deliberate: a fitted model is just a `fit()`
that returns a model object and a
[`predict()`](https://rdrr.io/r/stats/predict.html) that calls
[`predict()`](https://rdrr.io/r/stats/predict.html) on it. Two
consequences are already honoured here so that a model rule needs no
interface change — a non-integer visibility works end to end, and
`is_estimated` already drives the bootstrap path rather than waiting for
a model to appear.
