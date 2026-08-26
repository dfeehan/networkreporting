# Average estimates from several ties into one

Combines the ties' estimates cell by cell, as a weighted average.

## Usage

``` r
pool_ties(
  ...,
  estimator = c("ind", "agg"),
  weights = c("inverse-variance", "equal", "exposure"),
  method = c("auto", "replicate", "analytic")
)
```

## Arguments

- ...:

  estimator results, named by tie

- estimator:

  `"ind"` or `"agg"`

- weights:

  how to weight the ties: `"inverse-variance"` (the default), `"equal"`,
  or `"exposure"` (each tie's `denom.hat`)

- method:

  `"replicate"` to pool within bootstrap replicates, `"analytic"` for
  the inverse-variance formula, or `"auto"` (the default) to use
  replicates when every input has them

## Value

a `tie_pool`: a tibble with one row per cell

## The independence problem, and how to avoid it

The obvious way to pool is inverse-variance weighting, which assumes the
estimates being combined are **independent**. In a multi-tie survey they
are not: every tie is reported by the *same respondents*, so a
respondent weighted up perturbs every tie at once.

Where that correlation is positive — which is what shared respondents
usually produce — the independence formula gives an interval that is too
narrow. It is not guaranteed to err that way, though: with negatively
correlated ties it errs the other, and the size of the discrepancy is a
property of the data rather than something that can be reasoned out in
advance. The point is not the direction. It is that the formula is
answering a question about ties that do not exist.

There is a way round it that costs nothing extra if the estimates were
bootstrapped with the **same replicate weights**. Pool within each
replicate, then take the spread across replicates: whatever correlation
the ties have is already in there, because each replicate perturbs all
of them together. That is `method = "replicate"`, and it is the default
wherever the inputs allow it.

`method = "analytic"` is the inverse-variance formula, available for
when replicate estimates are not to hand. It warns, because the interval
it produces is not one you should quote without saying how it was made.

## Pooling is not the same as union

This treats each tie as a separate estimate of one quantity and averages
them. It does **not** add the ties' reports together — that is union, it
double-counts any alter reachable through more than one tie, and it
needs information this package does not carry. See
[`ties_union_check()`](http://dennisfeehan.org/networkreporting/reference/ties_union_check.md).

## See also

[`compare_ties()`](http://dennisfeehan.org/networkreporting/reference/compare_ties.md),
which is usually worth doing first

## Examples

``` r
if (FALSE) { # \dontrun{
pool_ties(siblings = sib_est, cousins = cousin_est)
} # }
```
