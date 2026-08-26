# Visibility from a clique tie

The exact rule, and the default everywhere. Reproduces
[`calculate_sib_ind_visibility()`](http://dennisfeehan.org/networkreporting/reference/calculate_sib_ind_visibility.md)
bit for bit: an alter who is on the frame has visibility `y.F`, and an
alter who is not has `y.F + 1`.

## Usage

``` r
vis_from_clique(ego.in.group = TRUE)
```

## Arguments

- ego.in.group:

  is ego a member of the group ego reports about? `TRUE` for siblings
  and households. Setting it `FALSE` drops the `+ 1`, which is what
  makes this the general clique rule rather than a sibling-specific one

## Value

a
[visibility_rule](http://dennisfeehan.org/networkreporting/reference/visibility_rule.md)

## Details

This is a *theorem*, not a definition, and it is worth being precise
about what buys it. It follows when three things hold:

- the tie partitions the population into disjoint groups (it is an
  equivalence relation, so it is transitive – siblingship is, cousinship
  is not);

- ego is a member of the group ego reports about (`ego.in.group`);

- reporting within the group is complete.

Given those, ego's own roster is sufficient to recover the visibility of
every alter, including alters whose neighbourhood ego cannot observe.
Siblings and households satisfy all three. Cousins, parents and
neighbours do not, and need
[`vis_from_donor()`](http://dennisfeehan.org/networkreporting/reference/vis_from_donor.md)
or a rule yet to be written.

The asymmetry between on-frame and off-frame alters is not a detail. A
dead alter is never on the frame, so every death is divided by
`y.F + 1`, while exposure is a mixture of both cases. That asymmetry is
the only way visibility survives into a rate, which is a ratio; a rule
that assigned one number to every alter in a cell would cancel out and
reduce exactly to the aggregate estimator.

## Examples

``` r
rule <- vis_from_clique()
rule
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
