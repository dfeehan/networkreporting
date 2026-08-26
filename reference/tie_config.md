# Declare what kind of tie a set of reports is about

A visibility rule is only valid for certain kinds of tie, and **which
kind a tie is cannot be worked out from the data**. `tie_config()` is
how the caller says so, and
[`apply_visibility_rule()`](http://dennisfeehan.org/networkreporting/reference/apply_visibility_rule.md)
refuses to apply a rule outside the structures it is valid for.

## Usage

``` r
tie_config(structure, name = NULL, ego.in.group = NULL, frame.indicator = NULL)

# S3 method for class 'tie_config'
print(x, ...)
```

## Arguments

- structure:

  one of `"clique"`, `"group"`, `"star"`, `"unbounded"`; see Details.
  There is no default: that is the point.

- name:

  optional label for the tie, used in provenance output

- ego.in.group:

  is ego a member of the group ego reports about? `TRUE` for siblings
  and household members; `FALSE` for parents, whose visibility is a fact
  about the sibship rather than the parent roster. `NULL` (the default)
  leaves it undeclared, in which case the rule's own setting is used

- frame.indicator:

  name of the 0/1 column saying whether each alter is in the frame
  population **for this tie**. `NULL` (the default) leaves it
  undeclared. Worth setting once ties differ in who is eligible to
  report or be reported about — neighbours bounded by a compound, say,
  against siblings who are not

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
networks, where the true reporting network is known and so is every
alter's true visibility.

Where the tie really is a clique, the rule is exact: on maternal
siblings, on maternal cousins and on paternal cousins it recovers the
true visibility of **100%** of alters, on both sides of the frame split.

Maternal cousins being a clique is worth pausing on, because it is the
opposite of what "cousinship is not transitive" suggests. Within one
line it *is* transitive: everyone sharing a maternal grandmother forms
an equivalence class. What breaks is the **union** of the two lines —
your maternal cousin and your paternal cousin are not each other's
cousins — and that is the genuine non-clique case. There the clique rule
overstates visibility by 1.089x for off-frame alters against 1.061x for
on-frame ones, and is exactly right for only 65% and 63% of them
respectively. Because a death is always off-frame while exposure is a
mixture, that 1.026x *differential* does not cancel out of a death rate;
it biases it.

So the lesson is not "cousins are not a clique". It is that whether a
particular roster is one is a question about how the roster was built,
which is why it has to be declared rather than guessed at from a name.

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

## Properties of the tie, not of the rule

`ego.in.group` and `frame.indicator` are facts about the *tie*, so this
is where they belong. Both default to `NULL`, meaning "not declared",
and a tie that declares neither behaves exactly as before.

Where a declared value meets one set somewhere else — `ego.in.group` on
[`vis_from_clique()`](http://dennisfeehan.org/networkreporting/reference/vis_from_clique.md),
or `frame.indicator` passed to
[`apply_visibility_rule()`](http://dennisfeehan.org/networkreporting/reference/apply_visibility_rule.md)
— **disagreement is an error, never a silent precedence.** Quietly
overriding one with the other would reintroduce the failure this class
exists to prevent: a number produced under an assumption the caller did
not know was in force. A value declared in only one place is simply
used.

## See also

[`vis_from_clique()`](http://dennisfeehan.org/networkreporting/reference/vis_from_clique.md),
[`apply_visibility_rule()`](http://dennisfeehan.org/networkreporting/reference/apply_visibility_rule.md)

## Examples

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
  # parents: ego is not a member of the group whose size sets visibility
  tie_config("star", name = "parents", ego.in.group = FALSE)
#> <tie_config>
#>   structure:       star
#>   name:            parents
#>   ego.in.group:    FALSE
#>   frame.indicator: (not declared)
```
