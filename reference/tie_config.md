# Declare what kind of tie a set of reports is about

A visibility rule is only valid for certain kinds of tie, and **which
kind a tie is cannot be worked out from the data**. `tie_config()` is
how the caller says so, and
[`apply_visibility_rule()`](http://dennisfeehan.org/networkreporting/reference/apply_visibility_rule.md)
refuses to apply a rule outside the structures it is valid for.

## Usage

``` r
tie_config(structure, name = NULL)

# S3 method for class 'tie_config'
print(x, ...)
```

## Arguments

- structure:

  one of `"clique"`, `"group"`, `"star"`, `"unbounded"`; see Details.
  There is no default: that is the point.

- name:

  optional label for the tie, used in provenance output

- x:

  a `tie_config`

- ...:

  unused

## Value

an object of class `tie_config`

## Why this is required rather than defaulted

[`vis_from_clique()`](http://dennisfeehan.org/networkreporting/reference/vis_from_clique.md)
computes `y.F` for an on-frame alter and `y.F + 1` otherwise. That is a
*theorem* about cliques, not a definition of visibility: it follows only
because the tie partitions the population into disjoint groups, ego
belongs to the group ego reports about, and reporting within the group
is complete.

Given a roster of reports, nothing distinguishes a tie that satisfies
those conditions from one that does not. The rule returns a finite,
plausible number either way. Applied to a non-clique tie it therefore
produces a wrong answer with no outward sign of trouble — which is why
the structure has to be stated rather than inferred or defaulted.

Checked against socsim ground truth on the Bangladesh reporting
networks: on siblings,
[`vis_from_clique()`](http://dennisfeehan.org/networkreporting/reference/vis_from_clique.md)
recovers the true visibility of **100%** of alters. On maternal cousins
— which are not a clique, since cousinship is not transitive — it
overstates visibility by 1.55x for off-frame alters and 1.29x for
on-frame ones. Because a death is always off-frame while exposure is a
mixture, that 1.20x *differential* does not cancel out of a death rate;
it biases it. Both cousin networks agree, so it is structural.

## The structures

- `"clique"`:

  An equivalence relation: mutual and transitive, so it partitions the
  population into disjoint groups, and ego is a member of the group ego
  reports about. Siblings and household members.

- `"group"`:

  Ego reports a group, but the relation is not an equivalence relation,
  so the groups overlap and ego's roster is not a clique. Cousins: your
  cousins need not be each other's cousins.

- `"star"`:

  Ego reports alters who form no group with one another, and whose
  visibility must come from somewhere other than this roster. Parents,
  whose visibility is the number of their living children on the frame —
  a fact about the *sibship*, not about the parent roster.

- `"unbounded"`:

  No bounded group at all, so there is no roster from which visibility
  could be derived. Neighbours, acquaintances.

## See also

[`vis_from_clique()`](http://dennisfeehan.org/networkreporting/reference/vis_from_clique.md),
[`apply_visibility_rule()`](http://dennisfeehan.org/networkreporting/reference/apply_visibility_rule.md)

## Examples

``` r
  tie_config("clique", name = "siblings")
#> <tie_config>
#>   structure: clique
#>   name:      siblings
  tie_config("group",  name = "maternal cousins")
#> <tie_config>
#>   structure: group
#>   name:      maternal cousins
```
