# Try visibility rules in priority order

Takes rules in priority order. For each row, the first rule returning a
non-`NA` visibility wins, and the row records which tier resolved it.

## Usage

``` r
vis_coalesce(...)
```

## Arguments

- ...:

  two or more
  [visibility_rule](http://dennisfeehan.org/networkreporting/reference/visibility_rule.md)
  objects, most preferred first

## Value

a
[visibility_rule](http://dennisfeehan.org/networkreporting/reference/visibility_rule.md)

## Details

This is the mixed case – some alters derivable exactly, some not – and
it is what turns "38% of reported cousins were approximated, 4% of them
from the global mean" into a number the package reports rather than an
assumption nobody wrote down.

Rules that would otherwise [`stop()`](https://rdrr.io/r/base/stop.html)
on an unresolved row are run as though `on_missing = "na"`, since
falling through is the entire point here.

## Examples

``` r
vis_coalesce(vis_from_clique(),
             vis_from_donor(match_on = c(.sib.sex = "sex")),
             vis_from_donor(match_on = NULL))
#> <visibility_rule: coalesce(clique > donor(.sib.sex) > donor(global))>
#>   requires:     y.F, .sib.in.F
#>   is_estimated: TRUE  (refit within each bootstrap replicate)
#>   parameters:
#>     tier1 = <clique>
#>     tier2 = <donor(.sib.sex)>
#>     tier3 = <donor(global)>
#>   assumptions:
#>     - tier 1 (clique): the tie partitions the population into disjoint groups
#>     - tier 1 (clique): ego is a member of the group ego reports about
#>     - tier 1 (clique): reporting within the group is complete
#>     - tier 2 (donor(.sib.sex)): visibility is borrowed from the survey respondents, matched on .sib.sex
#>     - tier 2 (donor(.sib.sex)): donor visibilities are summarised by their weighted harmonic mean
#>     - tier 2 (donor(.sib.sex)): donors are alive and on the frame, but many alters needing an imputed visibility are dead; where visibility correlates with mortality the donor is systematically wrong, not merely noisy
#>     - tier 3 (donor(global)): visibility is borrowed from the survey respondents, with no matching
#>     - tier 3 (donor(global)): donor visibilities are summarised by their weighted harmonic mean
#>     - tier 3 (donor(global)): donors are alive and on the frame, but many alters needing an imputed visibility are dead; where visibility correlates with mortality the donor is systematically wrong, not merely noisy
```
