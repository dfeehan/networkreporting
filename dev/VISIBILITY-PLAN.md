# Plan: extract a generic network-survival spine, and make visibility a first-class rule

Written 2026-08-25 in the Matlab mortality analysis repo, and set down here because most of the work
happens in **this** package. A pointer to it lives in `siblingsurvival/dev/PACKAGE-HANDOFF.md`.

For an agent working across **two existing repos**:

- **`dfeehan/networkreporting`** — `~/dev/networkreporting`, branch `master`, version 0.3.1. **This
  repo.** Where the generic spine lands, and where all of Phase 1 happens. Clean tree at writing.
- **`dfeehan/siblingsurvival`** — `~/dev/siblingsurvival`; `dhstest` is merged into `main`, so branch
  from `main`. Clean tree at writing.

Read the "Target package" section before touching anything: `networkreporting` is **not** a blank
slate, and the reconciliation there is the real work of Phase 0.

**A note on paths.** Unqualified `R/…`, `tests/…`, `vignettes/…` and `data-raw/…` references below
are to **`siblingsurvival`**, since that is where the code being moved currently lives. Anything in
this package is written `networkreporting/…`. Note also that `siblingsurvival`'s planning documents
moved out of its repo root into `siblingsurvival/dev/` in commit `5ebe8ec` — tracked, but
build-ignored — so `FUTURE-IMPROVEMENTS.md` and friends are now `dev/FUTURE-IMPROVEMENTS.md`.

Written in the idiom of `siblingsurvival/dev/PACKAGE-HANDOFF.md`: package-side only. The Matlab analysis repo
(`~/Dropbox/matlab-mortality`) is not modified here; the interface it will need is recorded at the end.

---

## Before you start

**You need write access to both repos.** Phase 0 is a two-repo operation — files leave
`siblingsurvival` as they arrive here, and `siblingsurvival`'s `DESCRIPTION`, `NAMESPACE`,
`R/globals.R` and a new `R/reexports.R` all change. If your session is scoped to
`~/dev/networkreporting` alone, add the other repo (`/add-dir ~/dev/siblingsurvival`) before
starting, or work from `~/dev`.

**Branch in both.** `networkreporting` is on `master`, `siblingsurvival` on `main`. Both trees were
clean when this was written. Neither package's changes make sense without the other's, so keep the
two branches in step and say so in both commit messages.

**Mind the install order — this will bite on the first test run.** Once `siblingsurvival` declares
`Imports: networkreporting`, it cannot load until a `networkreporting` build *containing the moved
spine* is installed, and no such build exists on CRAN. So the loop is always:

1. move code into `networkreporting`
2. `devtools::document()` + `devtools::install()` **networkreporting**
3. only then `devtools::load_all()` / `devtools::test()` **siblingsurvival**

An attempt to verify the `siblingsurvival` side first will fail with a missing-package error that
looks like a mistake in the move. It is not — it is step 2 not having happened yet. Do not "fix" it
by reverting.

**Sequencing against the rest of the handoff.** `siblingsurvival/dev/PACKAGE-HANDOFF.md` section F
argues this work should land *before* the remaining MICS items, not after: Phase 0 is a whole-file
move, which is cheapest on a quiet tree, and its verification gate (both validation harnesses
reproducing published figures unchanged) is strongest right now, while both harnesses pass and no
new work is in flight. Read F before deciding otherwise.

---

## Context

`siblingsurvival` estimates death rates from sibling histories. The estimator has four stages:

```
sib.dat
  → get_esc_reports()        row per (ego, sib, cell); occ/exp        [tie-agnostic]
  → add_esc_ind_vis()        adds y.F and ind_vis                     [SIBLING-SPECIFIC]
  → get_ec_reports()         collapses to row per (ego, cell)         [tie-agnostic]
  → get_ind_est_from_ec()    /  get_agg_est_from_ec()                 [tie-agnostic]
  → get_boot_ests_matrix()   replicate weights                        [tie-agnostic]
```

Only the second stage assumes anything about siblings. The rule it applies —
`vis = y.F` for an on-frame alter, `y.F + 1` otherwise (`R/get_sibship_visibility.R:371`) — is a
*theorem* about a specific tie structure, not a definition of visibility. It follows only because
siblingship is an equivalence relation (so it partitions the population into disjoint cliques),
ego is a member of the group it reports about, and reporting within the group is complete. Given
those three, ego's own roster is sufficient to recover the visibility of every alter, including
alters ego cannot observe the neighbourhood of.

Two things now push against that.

**Multi-tie work.** The Matlab study collects reports about siblings, household members, parents,
cousins, aunts/uncles, neighbours and acquaintances. Only siblings and households satisfy all three
conditions. Cousinship is not transitive, which is why `code/prep_pipeline/targets_cousins.R` carries
*four* competing visibility definitions tagged by an `alter_visibility_basis` column. Parents draw
their visibility from a *different* roster (the sibship). Neighbours punt with
`alter_visibility = 1`. All of this is hand-rolled outside the package, twice — once in `code/prep/`
and again in `code/prep_pipeline/`.

**Approximation is the normal case, and it is invisible.** For a non-clique tie, individual
visibility is generally not identified from one-sided reports. The standard move is to substitute a
summary of the *respondents'* visibility — overall, or within age/sex group. That substitution is a
real modelling assumption with a knowable bias, and it currently has nowhere to live in the API, so
it ends up as an undocumented `case_when` in analysis code.

**Intended outcome.** Visibility becomes a declared, composable, self-documenting object. Exact
derivation and approximation become two implementations of one interface, so an estimate can say
which one produced it and how much of it was approximated — and so the same cells can be re-estimated
under several rules to show the sensitivity.

### Decisions already taken

| Question | Decision |
|---|---|
| Where does the generic machinery live? | **`networkreporting`**, which `siblingsurvival` then depends on. Moving the ARD / scale-up code *out* of `networkreporting` into its own package is possible future work, not this plan |
| Staging | **Visibility rules first.** `tie_config` and the multi-tie estimator are a later plan |
| Frozen-visibility bootstrap | **Fix as part of this work** |
| Analysis-repo migration | **Out of scope**; record the interface only |

---

## A correction worth reading before you start

It is tempting to think a visibility that is constant within an estimation cell cancels out of the
rate, since the rate is a ratio and both numerator and denominator get divided by it. **It does not**,
and the reason determines the whole design of `vis_from_donor()` below.

Deaths and exposure do not receive the same visibility:

- A dead alter is never on the frame, so **every death** gets `1/(y.F + 1)`.
- Exposure is a **mixture**: living on-frame alters get `1/y.F`, living off-frame alters get `1/(y.F + 1)`.

So visibility survives in the rate purely through that asymmetry. An approximation that assigns one
number to every alter in a cell, without preserving the on-frame/off-frame split, reduces exactly to
the aggregate estimator and changes nothing.

`get_ec_reports()` already computes the sufficient statistics for this at `R/get_ec_reports.R:58-72`:
`y.Dcell`, `y.Ncell`, `y.NandFcell` (exposure from on-frame alters) and `y.NandnotFcell` (the rest).
**`y.NandFcell` and `y.NandnotFcell` are computed today and consumed nowhere.** They are what makes the
whole approximation layer cheap: with a group-size estimate `Ŝ` that is constant within a cell,

```
ind numerator   = y.Dcell / Ŝ
ind denominator = y.NandFcell / (Ŝ - 1)  +  y.NandnotFcell / Ŝ
```

with `Ŝ = ŷ.F + 1`. No per-sibling recomputation needed. This is also the key to the bootstrap fix.

(The occurrence side needs no split *for mortality* because a death is always off-frame. That is a
fact about mortality, not about the framework — see "Estimands that are not ratios" in Phase 1 for
the small generalisation that keeps this identity valid for totals and means too.)

---

## Phase 0 — move the spine into `networkreporting`

Nearly a pure move: the `siblingsurvival` side must come out behaviourally identical, verified by its
existing test suite and the DHS/MICS validation scripts. The `networkreporting` side has some
cleanup, described below.

### Target package: what you are moving *into*

`networkreporting` is the right home — its own Description already claims "network scale-up, indirect
sampling, network reporting, and sibling history" — but it is a dated codebase and there is
pre-existing overlap. Four things to deal with before moving code in.

**1. There is already a `network.survival.estimator()`.** `R/network_survival.r` (367 lines) holds
`network.survival.estimator()` and the exported `network.survival.estimator_()`. It is an *aggregate
multiplicity* estimator driven by respondents' own network-size estimates — i.e. the
aggregate-visibility branch, reached from the ARD/known-population side rather than from a roster.

**It is old and is to be superseded**, not merged. Concretely: leave it in place through Phase 0,
mark it deprecated (`.Deprecated()` pointing at the new entry point) once Phase 1 lands, and remove
it in a later release. Do not attempt to reconcile its internals with the spine — it does not share
the ego × sib × cell decomposition and trying to unify them will cost more than it returns. Do
capture, in the deprecation note, any behaviour it has that the spine cannot yet reproduce, so
nothing is lost silently.

**2. Style mismatch.** `networkreporting` is written in the older `plyr` / `reshape2` / `lazyeval`
idiom with `f()` / `f_()` NSE pairs; `siblingsurvival` is `dplyr` / `rlang`. Do **not** rewrite the
existing code to match — that is unbounded work. Move the spine in as-is (it is already dplyr/rlang)
and let the package hold both idioms for now. Add `rlang`, `purrr`, `forcats`, `tidyselect`, `Rcpp`
to `Imports` accordingly.

**3. `R/RcppExports.R` is stale.** It declares `resample_stratum` calling into
`networkreporting_resample_stratum`, but the package has **no `src/` directory** — that function
lives in `surveybootstrap` now, which is already in `Imports`. Delete the stale file *before*
adding real compiled code, or `Rcpp::compileAttributes()` will regenerate confusion on top of it.

**4. Roxygen version skew.** `networkreporting` is `RoxygenNote: 7.2.2`, `siblingsurvival` is
`7.3.3`. Bump and re-document the whole package in one commit, separate from any code move, so the
regenerated `NAMESPACE`/`man` churn does not hide real changes in review.

Also note `networkreporting` is a CRAN package and this adds compiled code to it for the first time
(`LinkingTo: Rcpp`, a `src/`, `useDynLib`). `SystemRequirements: GNU make` is already declared. That
is fine, but it makes the next CRAN submission a heavier one; worth knowing before starting.

### What moves

| From `siblingsurvival` | Contents |
|---|---|
| `R/occ_exp.R` | `occ.exp` |
| `src/compute_occ_exp.cpp`, `src/RcppExports.cpp`, `R/RcppExports.R` | `cpp_compute_occ_exp`, `cpp_compute_occ_exp2`, `window_intersect` |
| `R/cell_config.R` | `cell_config`, `make.age.groups`, `make.even.age.groups`, `make.time.periods`, `agenames` |
| `R/get_esc_reports.R` | `get_esc_reports` |
| `R/get_ec_reports.R` | `get_ec_reports` |
| part of `R/sibling_estimator.R` | `get_ind_est_from_ec`, `get_agg_est_from_ec`, `get_boot_ests_matrix` |
| part of `R/get_sibship_visibility.R` | `get_sibship_info`, `calculate_sib_ind_visibility`, `add_esc_ind_vis`, `get_visibility` — see the note below; these are clique-specific, not sibling-specific |
| `R/get_ic_reports.R` | `get_ic_reports`, `sib_ic_checks`, cell encode/decode helpers — these operate on `esc.dat`, nothing DHS-specific |
| `R/life_table.R` | `nmx_to_nqx`, `q15_to_50` |
| `tests/testthat/test_occ_exp.R`, `test_life_table.R` | move with their code |

### What stays

Everything that knows about DHS, MICS, or maternal mortality: `prep_dhs_sib_histories.R`,
`prep_mics_sib_histories.R`, `prep_nrsim_sib_histories.R`, `finalize_sib_prep.R`,
`check_varmap_cols.R`, `attributes_to_long.R`, `data.R` + `data/` (the varmaps),
`maternal_classification.R`, `add_maternal_deaths.R`, `maternal_estimators.R`,
`reproductive_age_groups.R`, `sibling_summ.R`, `get_ego_df.R`, `get_sib_df.R`, and
`sibling_estimator()` itself, which becomes a thin wrapper.

`get_ego_age_distn()` **stays** — it encodes the DHS age-standardisation convention and its long
`@section Details` block of validation evidence. Note that it currently lives in
`R/get_sibship_visibility.R` despite having nothing to do with visibility; this split fixes that
misfiling as a side effect.

### Are the `sibship` functions sibling-specific?

Their **names** are; their **content** is clique-specific, which is a strictly larger class. Worth
being precise, because it determines where they belong:

- `get_sibship_info()` counts group members on frame and adds one for ego. The `+ 1` is not about
  siblings — it encodes *ego is a member of the group it reports about*. That holds for households
  too, and fails for parents and neighbours.
- `calculate_sib_ind_visibility()` is `1/y.F` on-frame, `1/(y.F + 1)` otherwise. That is the clique
  rule exactly, with no sibling content at all.
- `get_visibility()` is the ego-level wrapper around the first of these, plus weighted summaries.

So they move — but they are not generic, and leaving them free-floating in `networkreporting` under
"sibship" names would misrepresent them as the general case when they are one rule among several.

**Phase 0 moves them verbatim**, names unchanged, so the move stays mechanically verifiable against
the existing tests. **Phase 1 then reframes them** as the private implementation of
`vis_from_clique()`: `get_sibship_info()` → `get_group_info(dat, ego.id, frame.indicator,
ego.in.group = TRUE)` with the `+ 1` becoming that explicit argument rather than a hardcoded
constant, and `calculate_sib_ind_visibility()` folded into the rule's `predict()` step. Do not
export the renamed internals. `get_visibility()` keeps its exported name and its current signature —
the Matlab socsim code calls it (`code/socsim/03_survey_interview.qmd:299`) — but its documentation
should say plainly that it implements the clique rule and name the assumptions that buys it.

### Mechanics

1. `LinkingTo: Rcpp`, `useDynLib`, and `importFrom(Rcpp, sourceCpp)` move to `networkreporting`.
   Drop them from `siblingsurvival`. `src/*.o` and `src/*.so` are build artifacts — confirm
   `src/.gitignore` covers them and do not copy them.
2. `siblingsurvival` gains `Imports: networkreporting`. Check for a dependency cycle: it must not be
   the case that `networkreporting` ends up importing `siblingsurvival`.
3. Add `R/reexports.R` to `siblingsurvival` re-exporting every moved **public** name, so no existing
   caller breaks: `occ.exp`, `cell_config`, `make.age.groups`, `make.even.age.groups`,
   `make.time.periods`, `nmx_to_nqx`, `q15_to_50`, `get_visibility`, `sib_ic_checks`. Roxygen idiom:

   ```r
   #' @importFrom networkreporting occ.exp
   #' @export
   networkreporting::occ.exp
   ```
4. `R/globals.R` splits: the `y.*cell` / `y.F` names go with `networkreporting`.
5. Check for **name collisions** in both directions before moving — `networkreporting` exports 19
   names today and has its own `helper_functions.r`, `summation.r`, `report_aggregator.r` and
   `variance_estimators.r`, any of which may already define an internal name the spine uses.
6. `networkreporting` already has `tests/testthat/` (`test_estimators.R`, `test_helpers.R`,
   `test_kp.R`, `test_variance.R`) plus an old-style `tests/test_all.r` runner. It does **not**
   declare `Config/testthat/edition: 3`, which `siblingsurvival` does and the moved tests assume —
   set it, and confirm the four existing test files still pass under edition 3 before moving anything
   in. Retire `tests/test_all.r` if it duplicates the testthat run.
7. `NEWS` is a plain file here, not `NEWS.md`. Either convert it or follow its existing format; do not
   silently start a second changelog.

### Phase 0 is done when

`devtools::test()` passes in both packages with **no test file edited except for its `library()`
line**, `R CMD check` is clean on both, and the DHS/MICS validation scripts reproduce every published
figure unchanged. Nothing in Phase 0 should require a single expected value to be updated; if one
does, stop and find out why before proceeding to Phase 1.

---

## Phase 1 — the visibility rule layer

All of this lands in `networkreporting`.

### The rule object

A rule is a **closure, not a table**, and it is split into **fit** and **predict**. The split is the
single most important decision in this plan, because it is what lets one interface cover exact
derivation, donor means, and — later — a fitted model, and what makes correct bootstrapping possible.

```r
structure(
  list(
    label        = "clique",
    requires     = c("y.F"),   # columns needed; checked up front against what exists
    is_estimated = FALSE,      # TRUE if fit() consumes the sample; see bootstrap below
    fit          = function(donor.dat, weights) { ... },        # → opaque state
    predict      = function(alter.rows, state) { ... }          # → vis, per row
  ),
  class = "visibility_rule"
)
```

`predict()` returns one row per row of `esc.dat` with three columns:

- `vis` — the visibility as a **count**. **May be non-integer** — a donor mean is, and a model
  prediction certainly is. Nothing downstream may assume integrality.
- `vis_weight` — `1 / vis`
- `vis_rule` — which rule produced this row, for provenance

The three families then differ only in what `fit()` does:

| Rule | `fit()` | `is_estimated` |
|---|---|---|
| `vis_from_clique()` | nothing — visibility is read off ego's own reports | `FALSE` |
| `vis_from_donor()` | weighted (harmonic) mean within `match_on` cells | `TRUE` |
| `vis_from_model()` *(Phase 2)* | `glm()` / any fitted object | `TRUE` |

`vis_from_model()` is **not** being built now. But the contract above is chosen so that it drops in
without disturbing anything: a model is just a `fit()` that returns a fitted object and a `predict()`
that calls `predict()` on it. Two consequences to honour in Phase 1 so this stays true —
non-integer `vis` must work end to end, and `is_estimated` must already drive the bootstrap path
rather than being inspected only when a model appears.

Storing both `vis` and `vis_weight` under distinct names is deliberate. The package currently calls
its reciprocal `ind_vis` while the Matlab pipeline calls its count `alter_visibility`; the two read
as the same thing and are inverses. Naming both ends this.

An internal `apply_visibility_rule(rule, esc.dat, sib.dat, ego.dat, weights)` validates `requires`
against the available columns and fails with a message naming what is present — follow the existing
up-front column check at `R/sibling_estimator.R:44-58`, which already does exactly this well.

### `vis_from_clique(ego.in.group = TRUE)`

Reproduces `calculate_sib_ind_visibility()` **bit for bit**. `vis = y.F` when the alter is on frame,
`y.F + 1` otherwise; `vis_weight = 1/vis`. Reuse `get_sibship_info()` unchanged for `y.F`.

This is the default everywhere, so nothing about existing behaviour moves.

### `vis_from_donor(...)`

The approximation, and the first `is_estimated = TRUE` rule. Borrow visibility from a donor
population, optionally matched on covariates. Read it as the simplest member of the predict-from-data
family: `fit()` is a grouped weighted mean, where a model's would be a regression.

```r
vis_from_donor(
  donor      = "egos",                  # "egos", or a data frame, or another tie's alters
  match_on   = c("sex", "age.cat"),     # NULL = one global value
  statistic  = "harmonic",              # "harmonic" | "arithmetic" | "median"
  donor_vis  = vis_from_clique(),       # how the donors' OWN visibility is derived
  min_donors = 25,
  on_missing = "error"                  # "error" | "fallback" | "na"
)
```

Four things the implementer must get right:

1. **Preserve the on-frame/off-frame split.** The donor summary yields an estimated group size
   `Ŝ` (equivalently `ŷ.F = Ŝ - 1`). Then `vis = ŷ.F` for an on-frame alter and `ŷ.F + 1` otherwise
   — *not* a single number for every alter in the cell. Per the correction above, dropping this
   collapses the estimator to the aggregate one.
2. **`statistic` defaults to `"harmonic"`.** The individual estimator averages `1/v`, so the
   functional that makes the plug-in unbiased is `(E[1/v])^-1`, not `E[v]`. Restore the weighted
   harmonic mean helper that `get_visibility()` used to carry (`wh.mean`, removed in `9afd617`) as an
   internal `whmean(x, w)`. `"arithmetic"` must remain available — it is what the historical
   `y.F.bar / (y.F.bar + 1)` adjustment factor used, and what
   `vignettes/sibling-estimates.Rmd:404` still computes by hand. Jensen puts harmonic ≤ arithmetic,
   so the two disagree in a known direction by an amount that grows with the variance of visibility.
   Say so in the docs.
3. **Donor coverage will fail, routinely.** `match_on` describes the *alter*; donors are respondents.
   DHS respondents are women 15–49, so an alter aged 60 has no donor cell. Today that surfaces as
   `NA` propagating silently into rates. `min_donors` + `on_missing = "error"` makes it loud. Perform
   the join *inside* the rule and key it on every `match_on` variable — `R/maternal_estimators.R:126`
   carries a comment recording a bug from keying on age alone and duplicating every row per sex;
   doing the join in one place retires that class of error.
4. **Document the direction of the bias.** Donors are alive and on-frame; a large share of the alters
   needing imputed visibility are dead. Where visibility correlates with mortality — via family size
   for kin ties, living arrangements for household ties — the donor is systematically wrong, not
   merely noisy. Have the constructor record this assumption in the provenance table so it reaches
   output rather than living only in a methods appendix.

### `vis_coalesce(...)`

Takes rules in priority order; for each row, the first rule returning a non-`NA` `vis` wins, and the
row records which tier resolved it.

```r
visibility = vis_coalesce(
  vis_from_clique(),                              # exact where it exists
  vis_from_donor(match_on = c("sex", "age.cat")), # age/sex-matched approximation
  vis_from_donor(match_on = NULL)                 # global mean, last resort
)
```

This is the mixed case — some alters derivable, some not — and it is what makes "38% of reported
cousins were approximated, 4% of them from the global mean" a number the package reports rather than
an assumption nobody wrote down.

### Provenance

`apply_visibility_rule()` returns a `vis_provenance` tibble alongside the values, carried through to
the estimator output as an attribute *and* summarised into a `visibility_basis` column on the
estimates:

- rule label per tier, and count of alters resolved at each
- donor counts per matched cell, and which cells fell below `min_donors`
- share of deaths, and share of exposure, that were approximated (these differ, and both matter)
- assumptions recorded by each rule

### Wiring into the estimator

`sibling_estimator()` gains one argument:

```r
sibling_estimator(..., visibility = vis_from_clique())
```

Replace the hard call to `add_esc_ind_vis()` at `R/sibling_estimator.R:83` with
`apply_visibility_rule()`, producing the same `ind_vis` column that `get_ec_reports(ind.vis.var =
'ind_vis')` already consumes. Everything downstream is untouched.

`add_esc_ind_vis()` stays, now implemented in terms of `vis_from_clique()`, so
`get_ec_reports(ind.vis.var = NULL)` keeps working.

### Estimands that are not ratios

A death rate is a ratio of two visibility-adjusted sums. Other estimands of interest — totals, means,
prevalences — are not, and the architecture should not assume they are.

**The good news: the visibility layer is already indifferent.** A rule produces a per-report
`vis_weight` and has no idea what is being summed. Nothing in Phase 1 needs to change for a total or
a mean, and no work should be invented here on that account. Ratio-ness is baked in exactly two
places, both downstream of visibility and both trivially separable when the time comes:
`asdr.hat = num.hat / denom.hat` in `get_ind_est_from_ec()` / `get_agg_est_from_ec()`
(`R/sibling_estimator.R:315, 353`) and the same line in `get_boot_ests_matrix()`. The right eventual
shape is that the estimator returns named visibility-adjusted **sums**, and forming a ratio from two
of them is a final, optional step.

**One thing to change now, because retrofitting it means touching the bootstrap path twice.**
`get_ec_reports()` currently splits *exposure* by frame status (`y.NandFcell` / `y.NandnotFcell`) but
not *occurrences*, because for mortality a death is always off-frame so the split is degenerate. That
degeneracy is specific to mortality. Generalise it: **split every reported quantity by frame status**,
giving `y.Dcell = y.DandFcell + y.DandnotFcell` alongside the existing exposure pair. Two payoffs:

- The cheap bootstrap identity below becomes uniform over quantities —
  `Σ_q ( q_F / (Ŝ - 1) + q_notF / Ŝ )` — rather than special-cased to occ/exp. Any future
  non-ratio estimand inherits it for free.
- For mortality, `y.DandFcell` must be **identically zero**. Assert it. A non-zero value means the
  frame indicator is miscoded — a data-quality check the package does not currently have, and one
  that would have caught at least one class of frame-definition error.

Keep the existing column names as they are so nothing downstream breaks; add the new ones alongside.

Today `get_boot_ests_matrix()` (`R/sibling_estimator.R:232`) reweights `y.Dcell.ind` and
`y.Ncell.ind`, which already have `ind_vis` baked in at `get_ec_reports()` time. Visibility is
therefore **frozen across replicates**. `R/maternal_estimators.R:95` compounds this: `vis_res` is
computed once and reused in the bootstrap branch at lines 228 and 238.

This is exactly what `is_estimated` is for. For `vis_from_clique()` freezing is *correct* — visibility
is a function of ego's own reports, not of which egos were sampled. **For any `is_estimated = TRUE`
rule it is wrong**: `Ŝ` is a sample quantity, and holding it fixed understates the variance. So the
replicate loop calls `fit()` again per replicate when `is_estimated`, and not otherwise.

Three cases, in increasing cost. Implement the first two now; leave the third as a guarded path so a
Phase 2 model rule has somewhere to land.

1. **`is_estimated = FALSE`.** Nothing changes. Fit once, reuse. This is every DHS/MICS estimate the
   package produces today.
2. **Estimated, and `vis` constant within cell** — which holds whenever `match_on ⊆ cell.vars`, the
   common case, since matching on alter sex and age group means matching on the cells themselves.
   `ec_dat` is already sufficient, via the two dormant columns:
   `num = y.Dcell / Ŝ_r`, `denom = y.NandFcell / (Ŝ_r - 1) + y.NandnotFcell / Ŝ_r`.
   One extra length-M vector per cell; no per-sibling recomputation. If `match_on ⊄ cell.vars` but is
   still discrete, add the donor-cell variables to the grouping in `get_ec_reports()` so the identity
   holds within (cell × donor cell), then sum over donor cells — still cheap.
3. **Estimated and not constant within any coarse grouping** — a model with continuous predictors.
   The identity above does not apply and the only correct route is to refit and re-predict at the
   `esc.dat` level inside each replicate, which is M times the cost of a point estimate. Detect this
   case, `warning()` with the projected cost, and let it run. Do not silently fall back to case 2:
   that would freeze the model and reintroduce the bug this section exists to fix.

**This change must be a no-op for `vis_from_clique()`.** That is what makes it safe to land: the
DHS/MICS validation numbers and their CIs must not move.

---

## Tests

Follow the house style in `tests/testthat/test_sibling_estimator.R`: construct `sib.dat` directly,
derive expected values analytically, and show the arithmetic in comments.

1. **Golden test.** `vis_from_clique()` reproduces current `sibling_estimator()` output exactly on the
   existing fixtures — point estimates *and* bootstrap CIs.
2. **Degenerate donor.** All egos share the same `y.F` ⇒ donor rule and clique rule agree exactly.
3. **Closed form.** `match_on ⊆ cell.vars` ⇒ result equals the
   `y.NandFcell / (Ŝ-1) + y.NandnotFcell / Ŝ` identity computed by hand.
4. **The split matters.** A rule that assigns one visibility to every alter regardless of frame status
   reproduces the *aggregate* estimator — assert this, as executable documentation of the correction
   above.
5. **Coverage failure.** An alter cell with no donors errors under `on_missing = "error"` and falls
   through under `"fallback"`; the fallback tier is recorded in provenance.
6. **Functional direction.** Harmonic ≤ arithmetic on a donor set with non-zero variance.
7. **Provenance completeness.** Tier counts sum to the number of alters; approximated shares of deaths
   and of exposure are both reported.
8. **Bootstrap.** Clique-rule CIs are byte-identical to current; donor-rule CIs are strictly wider
   than the same estimate with visibility frozen.
9. **Non-integer visibility.** A donor rule producing a fractional `Ŝ` runs end to end and gives the
   analytically expected answer. This is the guard rail for a future model rule, so it is worth an
   explicit test rather than relying on donor tests to happen to cover it.

---

## Verification, end to end

1. `devtools::test()` in both packages; `R CMD check` clean on both. In `networkreporting` that
   includes its four pre-existing test files, which must keep passing — the spine is arriving
   alongside that code, not replacing it.
2. **Re-run the existing validation harness** — this is the real regression test, and it already
   exists. From `siblingsurvival/data-raw/dhs-validation/`: `compare-package-to-reference.R` and
   `validate-allcause.R` against `published-targets.csv` and `allcause-results.csv`. Every published
   figure must be unchanged. Same for `data-raw/mics-validation/`. **This is the gate for Phase 0**:
   if these move, the move was not pure.
3. Confirm the deprecated `network.survival.estimator_()` still runs and returns what it did before,
   with the deprecation warning. Superseding it is not the same as breaking it.
4. Build the vignettes. `vignettes/sibling-estimates.Rmd:404` hand-computes
   `adj.factor = y.F.bar / (y.F.bar + 1)`; replace that passage with the equivalent
   `vis_from_donor(statistic = "arithmetic")` call and show that the numbers match, then show what
   `"harmonic"` gives instead.
5. Smoke-test the multi-tie direction on data that already exists: run the socsim reporting networks
   through `vis_coalesce(vis_from_clique(), vis_from_donor())` and check that the clique tier claims
   100% of sibling alters and that the donor tier activates for cousins.

## Documentation

- Changelog entries in both packages, in the style each already uses (`NEWS.md` in
  `siblingsurvival`, the plain `NEWS` file in `networkreporting`) — state what changed, what it
  defaults to, and what would move if a default were changed. The `siblingsurvival` entry should say
  plainly that the estimator internals now live in `networkreporting` and that every public name is
  re-exported, so a reader of that changelog does not go looking for deleted functions.
- `siblingsurvival/dev/FUTURE-IMPROVEMENTS.md` item 2 (the removed `adj.factor`, resolved 2026-08-25) should now point at
  `vis_from_donor(statistic = )` as the supported replacement for callers who want to rebuild one.
- New vignette, *Approximating visibility*, covering: why non-clique ties need it, the
  on-frame/off-frame subtlety, harmonic vs arithmetic, donor coverage failure, and reading the
  provenance table.

---

## Finding from verification step 5 (socsim, 2026-08-26)

The socsim smoke test was run against the Bangladesh reporting networks
(`matlab-mortality/code/socsim/04_visibility_rule_check.R`). Two results, and the
second changes Phase 2's priority.

**1. `vis_from_clique()` is exactly right on a clique.** On the sibling network,
100.0% of alters, on **both** sides of the frame split, get exactly their true
visibility. Theory confirmed against ground truth.

  A caveat that cost an hour: the raw sibling census carries 16,068 duplicate
  (ego, alter) pairs -- full siblings are linked once through each parent -- which
  double-counts `y.F` and makes the exact rule look 27% wrong. This is the open
  question in `20250205-refactor/simulate_surveys.qmd` ("why are there duplicate
  (.ego_id, alter_id) pairs?"). Dedup first; the answer is a data artifact, not a
  package defect.

**2. `vis_coalesce()` does NOT fall through for a non-clique tie, and the plan was
wrong to assume it would.** The expectation recorded below was that the donor tier
would activate for cousins. It does not, and it cannot: `vis_from_clique()` always
returns a finite number, because inapplicability is not detectable from the data.
The clique tier claims **100% of cousin alters**, and the provenance table then
reports `clique: 100%` -- which reads as "we used the exact rule" when the exact
rule did not apply.

That is the exact failure mode this architecture exists to prevent: a silent
assumption producing an unquestioned number.

**And the resulting error is differential, so it does not cancel.** Visibility
survives into a rate only through the on-frame/off-frame asymmetry. For cousins:

| network | off-frame (deaths) | on-frame (exposure) | differential |
|---|---|---|---|
| maternal cousins | 1.552x too high | 1.293x too high | **1.201** |
| paternal cousins | 1.562x too high | 1.277x too high | **1.223** |
| siblings | 1.000 | 1.000 | 1.000 |

Both cousin networks agree, so this is structural, not noise. A ~20% differential
biases a cousin-based death rate downward by roughly the same order. The exact
factor needs working through -- the exposure denominator is a mixture, so it is
not a clean ratio -- but the direction is unambiguous and the magnitude is not
small.

### Fixed, 2026-08-26

`tie_config()` was pulled forward out of Phase 2 and implemented, because the
current API would otherwise produce the biased cousin estimate above with a
provenance table that looked clean.

* `tie_config(structure, name)` --- `"clique"`, `"group"`, `"star"`,
  `"unbounded"`. No default; that is the point.
* Rules carry `applies_to`. `vis_from_clique()` is `"clique"`-only;
  `vis_from_donor()` makes no structural assumption and is valid anywhere.
* `apply_visibility_rule(..., tie = )` refuses a structure-restricted rule when
  no tie is declared, and refuses it against the wrong structure.
* `vis_coalesce()` **drops** tiers inapplicable to the declared tie, which is
  the fall-through this plan originally expected and did not get. Dropped tiers
  are named in the provenance output.
* `sibling_estimator(tie = )` defaults to `tie_config("clique", "siblings")`,
  so no existing estimate moves; both validation harnesses reproduce exactly.

**Still deferred:** the rest of what `tie_config()` was sketched to carry ---
`ego.in.group` (which stays on the rule for now), a per-tie `frame.indicator`,
and the multi-tie estimator. Only the applicability gate was pulled forward.

---

## Deliberately deferred to a Phase 2 plan

Do **not** build these now; they are recorded so the Phase 1 interfaces do not foreclose them.

> **Read this first if you are starting Phase 2.** Part of `tie_config()` has already been
> built --- it was pulled forward on 2026-08-26 because the socsim check above showed the
> API would otherwise produce a biased cousin estimate behind a clean-looking provenance
> table. See "Finding from verification step 5" for what landed. **Start from that code,
> not from a fresh design**, and check you are on a branch that contains it:
> `networkreporting` commit `87f36f2`, `siblingsurvival` commits `1235d6d` and `12a5ff8`.
> Building `tie_config()` again from this list will conflict with it.

- ~~`tie_config()` — declaring tie structure~~ **partly done.** `tie_config(structure, name)`
  exists, with `"clique"` / `"group"` / `"star"` / `"unbounded"`, and rules carry `applies_to`
  so an inapplicable one refuses rather than misleads. **Still open**, and both are real
  decisions rather than leftovers:
    * **`ego.in.group` was deliberately left on the rule**, not moved onto `tie_config`, to
      avoid two places to set it and a precedence question to get wrong. Moving it is
      defensible — it *is* a property of the tie — but it is a change, not a completion, and
      it needs a rule for what happens when the two disagree.
    * **A per-tie `frame.indicator`** is not built at all. It matters once ties differ in who
      is eligible to report or be reported about (neighbours bounded by bari, say).
- `network_survival_estimator()`. Note the shape assumed here has already drifted:
  `sibling_estimator()` now *takes* a `tie` argument defaulting to `tie_config("clique")`,
  rather than being a wrapper over a generic estimator. Either is workable; decide
  deliberately rather than inheriting this line.
- Combination across ties: **compare** (separate estimates, same cells), **union** (visibilities add
  only if alter sets are disjoint — and the package has no cross-tie alter identity today, `sib.id`
  being unique only within ego), **pool** (variance-weighted).
- `vis_from_other_group()` — the parents case, where visibility for tie A comes from tie B's roster.
- `vis_from_report()` — ego directly reports the alter's degree. A survey-design choice, and arguably
  the honest answer for non-clique ties.
- **`vis_from_model()`** — visibility predicted from a fitted model rather than a cell mean. The
  `fit`/`predict` contract and `is_estimated` in Phase 1 exist specifically so this needs no
  interface change: a new constructor and case 3 of the bootstrap path, nothing else.
- `vis_aggregate()` fed by ARD degree estimates, connecting to `networkreporting` and to
  `code/quantity_quality/02_ard_degree.Rmd` in the Matlab repo.
- **Splitting ARD / scale-up back out.** With the spine living in `networkreporting`, the plausible
  next structural move is the reverse of this one: lift the ARD / known-population / scale-up code
  (`scale_up.r`, `known_population.r`, `summation.r`, `indirect_sampling.r`, `rds.r`) into a package
  of its own that depends on the spine, leaving `networkreporting` as the spine plus its estimators.
  Not this plan's work. Nothing in Phase 1 should assume either arrangement.
- **Non-ratio estimands** — totals, means, prevalences. The visibility layer already supports them;
  what remains is separating ratio formation from sum formation in `get_ind_est_from_ec()`,
  `get_agg_est_from_ec()` and `get_boot_ests_matrix()`. Phase 1's frame-status generalisation of
  `get_ec_reports()` is the groundwork.
- **Retiring `network.survival.estimator()`.** Deprecated in Phase 1, removed in a later release.
- `true_visibility_from_network()` for simulation validation. The socsim code hand-rolls this three
  times today, and one of the three
  (`code/socsim/20250205-refactor/simulate_surveys.qmd:300`) computes the alter-side in-degree, which
  is a *different* quantity from `y.F` — a good argument for one canonical definition.

## Interface the Matlab analysis repo will need

Recorded per the `siblingsurvival/dev/ANALYSIS-REPO-CHANGES.md` convention; no changes to that repo
in this plan. Paths in this section are relative to `~/Dropbox/matlab-mortality`.

- `code/prep_pipeline/targets_siblings.R:86-111` and `targets_hh.R:102-132` are `vis_from_clique()`
  with `ego.in.group = TRUE`. Note their rosters **include the respondent as a row**
  (`targets_siblings.R:58-71`), so `*_num_in_*_and_F` is the package's `yprime.F`, not `y.F`.
- `targets_parents.R:99-101` is `vis_from_other_group(group = "sibling")` — Phase 2.
- `targets_cousins.R:309-530`'s four `alter_visibility_basis` values are four legitimate estimands,
  not four hacks. They map onto `vis_coalesce` chains once `tie_config` exists.
- `targets_neighbors.R:104` (`alter_visibility = 1`) is `vis_from_donor(match_on = NULL)` with a
  degenerate donor — worth re-expressing so the assumption is visible.
- Units: the repo's `alter_visibility` is a **count**; the package's `ind_vis` is its **reciprocal**.
  Migration must map `alter_visibility → vis`, not to `vis_weight`.
