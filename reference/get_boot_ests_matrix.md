# Fast bootstrap estimation using matrix multiplication

Replaces the wide-dataframe summarize_at + gather/spread approach with
direct matrix multiplication. For each cell, computes weighted sums
across all M bootstrap replicates simultaneously using BLAS routines,
avoiding the creation of 10k-column intermediate dataframes.

## Usage

``` r
get_boot_ests_matrix(
  ec_dat,
  boot_weights_df,
  ego_id_col,
  cell_vars,
  estimator_type,
  visibility = NULL,
  refit = NULL
)
```

## Arguments

- ec_dat:

  ego X cell data from get_ec_reports()

- boot_weights_df:

  dataframe with .ego.id column and boot_weight_1..M columns

- ego_id_col:

  name of the ego id column in ec_dat and boot_weights_df

- cell_vars:

  vector of column names defining cells (age, sex, time period, etc)

- estimator_type:

  either 'ind' (individual visibility) or 'agg' (aggregate visibility)

- visibility:

  optional
  [visibility_rule](http://dennisfeehan.org/networkreporting/reference/visibility_rule.md).
  If it is `is_estimated`, the rule is refit inside every replicate
  rather than frozen; see Details

- refit:

  optional `function(replicate_index)` returning the group size `S.hat`
  for each row of `ec_dat` under that replicate. Supplied by
  [`apply_visibility_rule()`](http://dennisfeehan.org/networkreporting/reference/apply_visibility_rule.md)-aware
  callers for an estimated rule

## Value

long-form data frame with one row per cell per bootstrap replicate

## Details

Visibility is normally baked into `y.Dcell.ind` and `y.Ncell.ind` at
[`get_ec_reports()`](http://dennisfeehan.org/networkreporting/reference/get_ec_reports.md)
time, which freezes it across replicates. For
[`vis_from_clique()`](http://dennisfeehan.org/networkreporting/reference/vis_from_clique.md)
that is *correct*: visibility is a function of ego's own reports, not of
which egos happened to be sampled.

For any rule with `is_estimated = TRUE` it is wrong. The estimated group
size is a sample quantity, and holding it fixed understates the
variance. So when such a rule is passed, each replicate recomputes the
estimate from the frame-split sufficient statistics that
[`get_ec_reports()`](http://dennisfeehan.org/networkreporting/reference/get_ec_reports.md)
already produces:

\$\$num = y.DandFcell / (S - 1) + y.DandnotFcell / S\$\$ \$\$denom =
y.NandFcell / (S - 1) + y.NandnotFcell / S\$\$

which needs one length-M vector per cell and no per-alter recomputation.
This holds whenever the estimated visibility is constant within a cell,
which is the common case, since matching on alter sex and age group
means matching on the cells themselves.
