# True visibility, read off a known reporting network

Counts, for each alter, how many frame-population members are connected
to them — which is exactly what a visibility rule is trying to estimate.
Only usable where the whole network is known, so in practice:
simulation.

## Usage

``` r
true_visibility_from_network(
  census,
  reporter,
  alter,
  reporter.in.frame = NULL,
  dedup = TRUE
)
```

## Arguments

- census:

  a data frame with one row per (reporter, alter) tie in the true
  network

- reporter:

  name of the column identifying the person who could report

- alter:

  name of the column identifying the person reported about

- reporter.in.frame:

  name of a 0/1 or logical column saying whether the **reporter** is in
  the frame population, or `NULL` if every row is already a frame-member
  reporter

- dedup:

  drop repeated (reporter, alter) pairs before counting

## Value

a tibble with one row per alter: the alter id and `vis_true`

## Whose frame membership

`reporter.in.frame` names a column describing the **reporter**, not the
alter. This is the distinction that gets lost when the definition is
written inline, and it is not a technicality: an alter's visibility is
the number of people who *could report them*, so it depends on who is in
the frame population and connected to them. Filtering on the alter's own
frame status instead yields a different quantity — one that is also a
plausible number, and also wrong.

If the census has already been restricted to frame-member reporters,
leave `reporter.in.frame` as `NULL` and every row counts.

## Duplicate ties

A census may carry the same (reporter, alter) pair more than once — full
siblings get linked once through each parent, so every sibling pair
appears twice. Counting rows then doubles the visibility. `dedup = TRUE`
removes repeated pairs and reports how many it dropped; the count is
worth looking at, since a large one usually means the census is built
differently from how you thought.

## See also

[`visibility_accuracy()`](http://dennisfeehan.org/networkreporting/reference/visibility_accuracy.md),
which scores a rule against this

## Examples

``` r
census <- data.frame(
  from = c("a", "b", "c", "a"),
  to   = c("x", "x", "x", "y"),
  from_in_frame = c(1, 1, 0, 1))
# x is connected to three people, but only two of them are in the frame
true_visibility_from_network(census, "from", "to", "from_in_frame")
#> # A tibble: 2 × 2
#>   to    vis_true
#>   <chr>    <int>
#> 1 x            2
#> 2 y            1
```
