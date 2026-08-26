# Can these ties be unioned?

Union is the third combination: treat several ties as one larger tie and
estimate once from their pooled reports. Unlike
[`compare_ties()`](http://dennisfeehan.org/networkreporting/reference/compare_ties.md)
and
[`pool_ties()`](http://dennisfeehan.org/networkreporting/reference/pool_ties.md)
it cannot be done from finished estimates, because it changes what is
estimated rather than combining answers — the reports have to be pooled
and the visibility recomputed on the union.

## Usage

``` r
ties_union_check(..., alter.key = NULL)
```

## Arguments

- ...:

  report-level data frames, named by tie. Usually the `esc.dat` from
  each
  [`network_survival_estimator()`](http://dennisfeehan.org/networkreporting/reference/network_survival_estimator.md)
  result

- alter.key:

  name of a column identifying an alter *across* ties

## Value

a `ties_union_check`, invisibly; printed for its report

## Details

It also needs something the package does not carry by default: **an
identity for alters that holds across ties.** An alter reachable through
two ties must be recognised as one alter, or their reports are counted
twice and their visibility computed as though two separate people. Alter
ids in this package are unique only within an ego, which is not enough.

This function checks whether a supplied key does the job, and reports
what union would involve. It does not perform the union: once the key
exists, the operation is to bind the report rows, deduplicate on the
key, and estimate once with a
[`tie_config()`](http://dennisfeehan.org/networkreporting/reference/tie_config.md)
describing the union — which is usually *not* the structure of either
part. Two cliques unioned are generally not a clique.

## See also

[`pool_ties()`](http://dennisfeehan.org/networkreporting/reference/pool_ties.md),
which averages estimates instead and needs no such key

## Examples

``` r
if (FALSE) { # \dontrun{
ties_union_check(maternal = a$esc.dat, paternal = b$esc.dat,
                 alter.key = "person_id")
} # }
```
