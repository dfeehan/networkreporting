Future improvements to `networkreporting`
====

A running list of things worth doing to the package that are *not* defects. Each
is either a capability the package does not have, or a decision that should be
made deliberately rather than defaulted into.

Started 2026-08-26, at the end of the visibility work in `VISIBILITY-PLAN.md`.
That plan's "Deliberately deferred" section has been worked through; what is left
of it lives here, along with things the work surfaced along the way.

Ordered roughly by how much they would matter.


1. Split the ARD / scale-up code into its own package
----

**Status: open. The largest structural item, and the one most worth doing.**

The package now contains two things that share a name and very little else:

* the **network survival spine** --- `occ.exp()`, `cell_config()`,
  `get_esc_reports()`, `get_ec_reports()`, the estimators, and the whole
  visibility rule layer;
* the **ARD / known-population / scale-up** code --- `scale_up.r`,
  `known_population.r`, `summation.r`, `indirect_sampling.r`, `rds.r`.

They are written in different idioms (`plyr`/`reshape2`/`lazyeval` with `f_()`
NSE pairs, against `dplyr`/`rlang`), they are tested separately, and outside
`vis_aggregate()` they do not call each other. `VISIBILITY-PLAN.md` records that
Phase 0 was the reverse of this move and that "nothing in Phase 1 should assume
either arrangement", which is still true.

**What has changed since that was written** is that the spine has grown a great
deal --- five visibility rules, a generic estimator, cross-tie combination --- so
the imbalance is larger than it was. A user who wants the scale-up estimators now
installs Rcpp, a `src/`, and the entire visibility layer to get them.

**The one real coupling** is `vis_aggregate()`, which takes degree estimates
produced by `kp.degree.estimator()`. Note it takes them as a *column of numbers*,
not by calling the estimator, so the dependency is conceptual rather than
structural: a split would leave `vis_aggregate()` documented in terms of a
function in the other package, which is untidy but not broken.

Deciding this needs a view on who the users are, which is not a question the code
can answer.


2. Means and prevalences
----

**Status: open, and it is not just plumbing.**

Totals were done on 2026-08-26: the visibility-adjusted sums now carry their own
uncertainty and `estimated_total()` reads one out. That works because the
quantity being summed --- occurrences, exposure --- is already in `ec.dat`.

A *mean* or *prevalence* needs the numerator to be some **other** variable:
income, a binary attribute, anything the analyst names. That variable has to be
carried through `get_esc_reports()` and summed by `get_ec_reports()`, and neither
takes a caller-named quantity today. The work is upstream of the estimators
rather than in them, which is why it did not fall out of the totals change.

Worth knowing before starting: `get_ec_reports()` already splits every quantity
it sums by frame status (`y.DandFcell` / `y.DandnotFcell` alongside the exposure
pair), so an arbitrary summed variable would inherit the frame-split machinery
and the cheap bootstrap identity for free. The generalisation was done with this
in mind.


3. Union across ties
----

**Status: blocked, and the blocker is real rather than an oversight.**

`compare_ties()` and `pool_ties()` combine *results*. Union combines *data*: it
pools the reports and re-estimates, so it changes what is estimated rather than
combining answers, and cannot be done from finished estimates at all.

It needs **an identity for alters that holds across ties**. An alter reachable
through two ties has to be recognised as one alter, or their reports are counted
twice and their visibility computed as though they were two people. Alter ids in
this package are unique only within an ego.

`ties_union_check()` reports whether a supplied key does the job and measures the
overlap. What it deliberately does not do is perform the union, because the
remaining step is a modelling decision rather than a mechanical one: **the union
of two ties is generally not the structure of either part**, so it needs its own
`tie_config()`. Two cliques unioned are usually not a clique --- which is exactly
what the socsim maternal/paternal cousin measurement shows.


4. Retire `network.survival.estimator()`
----

**Status: deprecated since 0.3.2; removal still blocked.**

It is an aggregate multiplicity estimator driven by respondents' own network-size
estimates, superseded by the spine. Two things it does that the spine still
cannot:

* **It estimates from aggregate reports plus known populations, with no ego X
  alter roster.** `vis_aggregate()` closed half of this in 0.3.2 --- ARD degrees
  can now supply the spine's visibility --- but the spine still needs a roster to
  attach that visibility to. A survey with aggregate counts and no roster has no
  way in.
* **`within.alter.weights`**, which corrects for respondents who report about only
  a subset of their alters. There is no equivalent.

Removing it before those are addressed would lose capability silently, which is
what the deprecation message says rather than promising a date.


5. The *Approximating visibility* vignette is a draft
----

**Status: moved to `vignettes/draft/` on 2026-08-26, so it no longer ships or is
checked.**

It covers the whole rule family --- clique, group size, donor, model, report,
aggregate, coalescing, provenance --- and its worked examples all ran when it was
moved. What makes it a draft rather than a shipped vignette is length and shape:
it grew a section per rule as the rules were built, which is the order they were
written in rather than the order a reader needs them.

**One thing to be aware of.** While it was a vignette its code ran under
`R CMD check`, and that caught real things --- the `all.equal()` check that
`vis_from_donor(statistic = "arithmetic")` reproduces the historical
`adj.factor`, for one. Out of the build, nothing runs it. The substantive claims
are also covered by unit tests (`test_visibility_rules.R` asserts the flat-rule /
aggregate-estimator equivalence, and harmonic <= arithmetic), so the loss is
modest, but it is a loss.

Worth either reshaping it around what a reader needs to decide --- which rule for
which tie, and what each assumes --- or splitting it into two.


6. `vis_from_other_group()` as a named constructor
----

**Status: mechanism built, name not.**

The parents case --- where visibility for tie A comes from tie B's roster --- is
expressible today: compute the sibship size and pass it to
`vis_from_group_size(size.var)`. What does not exist is a constructor that names
the operation.

The reason it was not built is that it has to decide **how a caller identifies
"tie B"**. By name, once ties are first-class objects that the estimator knows
about --- which is the multi-tie estimator's problem, not this rule's. Building
the name before that decision would fix the wrong interface.


7. `ego.in.group` and the `"clique"` structure can contradict each other
----

**Status: deliberate, and worth revisiting only with evidence.**

`tie_config("clique", ego.in.group = FALSE)` is accepted, even though the
documented definition of `"clique"` includes ego belonging to the group. The
check was built and then removed: enforcing it made
`vis_from_clique(ego.in.group = FALSE)` unreachable, since a clique tie would
refuse `FALSE` and every non-clique tie refuses the clique rule.

The configuration is real --- a household roster that excludes the respondent is
a clique the respondent is outside of, and the Matlab rosters carry the
respondent as a row, so their count is `yprime.F` rather than `y.F`. If a
structure name for "a clique ego is not in" ever seems worth having, that is the
change to make, not re-adding the check.


8. Housekeeping
----

Small, and none of it urgent.

* **Two pre-existing `R CMD check` NOTEs.** Global-variable bindings in
  `kp.estimator_()` and `network.survival.estimator_()`, and lost braces in
  `nsum.estimator.Rd:38`. Both predate the visibility work and are in the
  scale-up half; item 1 would take them with it.
* **`alter.sex` is one covariate with a specific name.** The generic estimator
  takes a single alter attribute that enters the cells alongside age and time
  period, called `alter.sex` because sex is what it is in every current
  application. Carrying several such attributes, rather than one plus
  `cell.config$covars`, is a tidier design and a real change.
* **`vis_coalesce()` fits every tier up front**, including tiers no row will
  reach, so a chain whose fallback is a donor rule needs donor data even when the
  first tier resolves everything. Harmless but surprising.
* **The internal column names are sibling-flavoured.** `.sib.in.F`, `.sib.id` and
  `.sib.sex` are the convention throughout the spine and the rule layer, in a
  package where the estimator is now generic. Renaming them is mechanical, wide,
  and touches every rule's `requires`.
