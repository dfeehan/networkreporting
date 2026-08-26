# Visibility from a group size the caller supplies

[`vis_from_clique()`](http://dennisfeehan.org/networkreporting/reference/vis_from_clique.md)
works out the group by counting ego's roster. Sometimes the group whose
size sets an alter's visibility is *not* the roster the alter was
reported on, and only the caller can say what it is. This rule takes
that count from a column instead of deriving it.

## Usage

``` r
vis_from_group_size(
  size.var,
  counts.ego = TRUE,
  subtract.self = TRUE,
  ego.in.group = TRUE,
  label = NULL
)
```

## Arguments

- size.var:

  name of the column giving the number of frame-population members in
  the alter's group

- counts.ego:

  does `size.var` already count ego? Rosters that carry the respondent
  as a row do, in which case the column is the package's `yprime.F`
  rather than `y.F`. When `FALSE` and ego belongs to the group, one is
  added

- subtract.self:

  subtract one for an alter who is themselves in the frame population,
  since they cannot report themselves. `TRUE` unless you have a reason

- ego.in.group:

  is ego a member of the group being sized? Only consulted when
  `counts.ego` is `FALSE`. May be declared on the
  [`tie_config()`](http://dennisfeehan.org/networkreporting/reference/tie_config.md)
  instead

- label:

  optional short name for this basis, used in provenance. Worth setting
  when comparing several bases, so the output says which produced which
  estimate

## Value

a
[visibility_rule](http://dennisfeehan.org/networkreporting/reference/visibility_rule.md)

## When the group is not the roster

Three cases, all real:

- Pooled ties:

  Cousins are reported through the maternal and the paternal side
  separately, but an alter's visibility depends on everyone who could
  have reported them — both sides, plus ego's own siblings. That pooled
  group is not any single roster.

- Nested ties:

  "Pooled cousins, excluding ego's siblings" is the pooled group minus
  the sibship. A difference of two counts, which no roster holds
  directly.

- Borrowed groups:

  A parent's visibility is the number of their children on the frame — a
  fact about the *sibship*, not about the parent roster the parent was
  reported on. This is the mechanism the parents case needs.

## What this rule does and does not assume

It makes **no structural assumption**, so it needs no `tie` and is valid
for any structure. That is not because it is safe in the way
[`vis_from_donor()`](http://dennisfeehan.org/networkreporting/reference/vis_from_donor.md)
is safe — it is because the caller has taken responsibility for the part
[`vis_from_clique()`](http://dennisfeehan.org/networkreporting/reference/vis_from_clique.md)
would have derived. The package can check the arithmetic; it cannot
check that the column counts the right people.

What it still does for you is the frame split, which is easy to get
wrong and matters more than it looks. An alter who is themselves in the
frame population cannot report themselves, so their visibility is one
lower than the group total. Since a death is always off-frame while
exposure is a mixture, that asymmetry is the only route by which
visibility reaches a rate. Set `subtract.self = FALSE` only
deliberately.

## See also

[`vis_from_clique()`](http://dennisfeehan.org/networkreporting/reference/vis_from_clique.md),
which derives the group instead of taking it

## Examples

``` r
# a roster that carries the respondent as a row, so the count includes ego
vis_from_group_size("n_in_cousinship_and_F")
#> <visibility_rule: group_size(n_in_cousinship_and_F)>
#>   requires:     n_in_cousinship_and_F, .sib.in.F
#>   is_estimated: FALSE  (fit once; frozen across bootstrap replicates)
#>   parameters:
#>     size.var = n_in_cousinship_and_F
#>     counts.ego = TRUE
#>     subtract.self = TRUE
#>     ego.in.group = TRUE
#>     label = NULL
#>   assumptions:
#>     - visibility is taken from the supplied column 'n_in_cousinship_and_F', which the caller has computed; the package does not check that it counts the right people
#>     - that column already counts ego
#>     - an alter in the frame population does not count themselves

# the same, named for provenance
vis_from_group_size("n_in_pooled_and_F", label = "pooled cousins")
#> <visibility_rule: pooled cousins>
#>   requires:     n_in_pooled_and_F, .sib.in.F
#>   is_estimated: FALSE  (fit once; frozen across bootstrap replicates)
#>   parameters:
#>     size.var = n_in_pooled_and_F
#>     counts.ego = TRUE
#>     subtract.self = TRUE
#>     ego.in.group = TRUE
#>     label = pooled cousins
#>   assumptions:
#>     - visibility is taken from the supplied column 'n_in_pooled_and_F', which the caller has computed; the package does not check that it counts the right people
#>     - that column already counts ego
#>     - an alter in the frame population does not count themselves
```
