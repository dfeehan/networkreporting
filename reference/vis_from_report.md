# Visibility as reported by the respondent

Ego is asked, about each alter, how many frame-population members that
alter is connected to. The answer is the alter's visibility, read
straight off the questionnaire instead of derived from a roster or
borrowed from donors.

## Usage

``` r
vis_from_report(
  report.var,
  counts.ego = TRUE,
  counts.self = FALSE,
  on_missing = c("error", "na"),
  on_impossible = c("error", "floor", "na"),
  label = NULL
)
```

## Arguments

- report.var:

  name of the column holding ego's reported count for each alter

- counts.ego:

  did the respondent count themselves? When `FALSE`, one is added back

- counts.self:

  did the answer count the alter themselves? When `TRUE`, one is
  subtracted for an alter who is in the frame population, since an alter
  cannot report themselves

- on_missing:

  what to do about an alter with no reported value: `"error"` or `"na"`
  (leave unresolved, so
  [`vis_coalesce()`](http://dennisfeehan.org/networkreporting/reference/vis_coalesce.md)
  can try the next tier)

- on_impossible:

  what to do about a reported visibility below one: `"error"`, `"floor"`
  (raise it to one) or `"na"`. An alter who was reported was, by
  construction, visible to at least one person, so a zero is a data
  problem rather than a small number

- label:

  optional short name for provenance

## Value

a
[visibility_rule](http://dennisfeehan.org/networkreporting/reference/visibility_rule.md)

## Why this is the honest option for some ties

Every other rule here recovers visibility from something the survey
happened to collect for another purpose.
[`vis_from_clique()`](http://dennisfeehan.org/networkreporting/reference/vis_from_clique.md)
exploits the tie's structure,
[`vis_from_donor()`](http://dennisfeehan.org/networkreporting/reference/vis_from_donor.md)
and
[`vis_from_model()`](http://dennisfeehan.org/networkreporting/reference/vis_from_model.md)
substitute the respondents' own. All three are attempts to get at a
quantity nobody was asked about.

For an `"unbounded"` tie there is no way round that. Neighbours and
acquaintances have no bounded group to count, so there is no roster to
derive from and nothing that makes the respondents a good stand-in for
the alters. Asking is the only route to the quantity, and this rule is
what turns the answer into an estimate.

It is a **survey-design choice** as much as an analysis one: it costs
questionnaire time, and it can only be used if somebody decided in
advance to ask. Where the question was asked, though, it beats an
approximation derived from something else — and unlike the other rules,
it puts the uncertainty somewhere visible, in reporting error rather
than in an assumption.

## Two things the answer may or may not include

The wording of the question decides both, and the package cannot tell
from the numbers which was meant.

- `counts.ego` — did the respondent count *themselves* among the alter's
  connections? "How many people like you does X know?" usually includes
  them; "how many *other* people like you" does not, and then one has to
  be added back.

- `counts.self` — did the answer count the alter? A question phrased
  about a *group* ("how many people are in X's household?") does; one
  phrased about connections does not. When it does, this rule behaves
  like
  [`vis_from_group_size()`](http://dennisfeehan.org/networkreporting/reference/vis_from_group_size.md),
  which is the better constructor to reach for.

## See also

[`vis_from_group_size()`](http://dennisfeehan.org/networkreporting/reference/vis_from_group_size.md),
for a reported *group size* rather than a reported degree

## Examples

``` r
vis_from_report("n_known_by")
#> <visibility_rule: reported(n_known_by)>
#>   requires:     n_known_by, .sib.in.F
#>   is_estimated: FALSE  (fit once; frozen across bootstrap replicates)
#>   parameters:
#>     report.var = n_known_by
#>     counts.ego = TRUE
#>     counts.self = FALSE
#>     on_missing = error
#>     on_impossible = error
#>     label = NULL
#>   assumptions:
#>     - visibility is taken from the respondent's reported count in 'n_known_by'
#>     - the reported count includes the respondent
#>     - the reported count excludes the alter
#>     - respondents report their alters' connections accurately; reporting error in this question passes straight into the estimate, where the other rules would instead carry an assumption
vis_from_report("n_other_known_by", counts.ego = FALSE)
#> <visibility_rule: reported(n_other_known_by)>
#>   requires:     n_other_known_by, .sib.in.F
#>   is_estimated: FALSE  (fit once; frozen across bootstrap replicates)
#>   parameters:
#>     report.var = n_other_known_by
#>     counts.ego = FALSE
#>     counts.self = FALSE
#>     on_missing = error
#>     on_impossible = error
#>     label = NULL
#>   assumptions:
#>     - visibility is taken from the respondent's reported count in 'n_other_known_by'
#>     - the reported count excludes the respondent, who is added back
#>     - the reported count excludes the alter
#>     - respondents report their alters' connections accurately; reporting error in this question passes straight into the estimate, where the other rules would instead carry an assumption
```
