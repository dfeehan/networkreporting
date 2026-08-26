# Estimate death rates from network reporting data

The generic network survival estimator. Given reports about alters
connected to survey respondents by some tie, it produces age-specific
rates using both the individual-visibility and the aggregate-visibility
estimators.

## Usage

``` r
network_survival_estimator(
  rel.dat,
  ego.id,
  alter.id,
  frame.indicator,
  alter.sex = "sex",
  cell.config,
  weights,
  boot.weights = NULL,
  return.boot = FALSE,
  visibility = vis_from_clique(),
  tie,
  discretize.exp = FALSE,
  .arg.labels = character(0),
  .data.label = "rel.dat"
)
```

## Arguments

- rel.dat:

  The long-form ego X alter dataset: one row per reported alter, per ego

- ego.id:

  String naming the column of `rel.dat` with the survey respondent's id

- alter.id:

  String naming the column of `rel.dat` with the alter's id

- frame.indicator:

  String naming the 0/1 column of `rel.dat` saying whether each alter is
  in the frame population

- alter.sex:

  String naming the alter attribute that enters the estimation cells
  alongside age and time period. Called `alter.sex` because sex is what
  it is in every current application; carrying several such attributes,
  rather than one plus `cell.config$covars`, is future work

- cell.config:

  An object from
  [`cell_config()`](http://dennisfeehan.org/networkreporting/reference/cell_config.md)
  configuring the cells

- weights:

  String naming the column of `rel.dat` with the sampling weight

- boot.weights:

  Optional dataframe of bootstrap resampled weights; see Details

- return.boot:

  If TRUE, and `boot.weights` is given, return every bootstrap estimate
  rather than only their summaries

- visibility:

  A
  [visibility_rule](http://dennisfeehan.org/networkreporting/reference/visibility_rule.md)
  saying how each alter's visibility is derived. Defaults to
  [`vis_from_clique()`](http://dennisfeehan.org/networkreporting/reference/vis_from_clique.md),
  which is exact for a clique tie and refuses any other structure

- tie:

  A
  [`tie_config()`](http://dennisfeehan.org/networkreporting/reference/tie_config.md)
  saying what kind of tie these reports are about. **Required**; see
  above

- discretize.exp:

  Boolean for whether exposure should be discretized. Not yet
  implemented

- .arg.labels:

  Internal. Named character vector letting a wrapper phrase the up-front
  column-check message in its own argument names

- .data.label:

  Internal. Name to use for the data argument in that message

## Value

a list with `asdr.ind` (individual-visibility estimates), `asdr.agg`
(aggregate-visibility estimates), `ec.dat`, `esc.dat`, and a
`vis_provenance` object saying how visibility was arrived at

## Details

`siblingsurvival::sibling_estimator()` is this function with the sibling
names and the clique tie filled in; if you are working with sibling
histories, use that.

## The tie is required

There is no default `tie`, and that is the point. Which kind of tie a
set of reports is about cannot be read off the data: on a tie that is
not a clique, the default
[`vis_from_clique()`](http://dennisfeehan.org/networkreporting/reference/vis_from_clique.md)
rule still returns a finite, plausible number, and it can be wrong.
Measured against socsim ground truth on a roster that pools maternal and
paternal cousins — which is not a clique, even though each line
separately is — it overstates visibility by 1.089x for off-frame alters
against 1.061x for on-frame ones. Because a death is always off-frame
while exposure is a mixture, that differential biases the rate rather
than cancelling out of it. See
[`tie_config()`](http://dennisfeehan.org/networkreporting/reference/tie_config.md).

## Details

If you want estimated sampling variances, pass a data frame
`boot.weights`. It is assumed to have a column named whatever `ego.id`
is, and then columns `boot_weight_1`, ..., `boot_weight_M`.

## See also

`siblingsurvival::sibling_estimator()`,
[`tie_config()`](http://dennisfeehan.org/networkreporting/reference/tie_config.md),
[`vis_from_clique()`](http://dennisfeehan.org/networkreporting/reference/vis_from_clique.md),
[`vis_from_donor()`](http://dennisfeehan.org/networkreporting/reference/vis_from_donor.md)
