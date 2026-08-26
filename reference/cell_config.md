# Specify the cells (age groups, time period) to produce estimates for

Specify the cells (age groups, time period) to produce estimates for

## Usage

``` r
cell_config(
  age.groups,
  time.periods,
  start.obs,
  end.obs,
  event,
  age.offset,
  time.offset,
  covars = NULL,
  event.name = NULL,
  exp.scale = 1/12
)
```

## Arguments

- age.groups:

  see Details

- time.periods:

  see Details

- start.obs:

  the column name with the time observation started

- end.obs:

  the column name with the time observation ended

- event:

  the column name with the time of the event (e.g. date of death)

- age.offset:

  the column name with each sibling's offset for age (typically the date
  of birth)

- time.offset:

  the column name with each sibling's offset for time (often the date of
  the survey interview)

- covars:

  (optional) vector with names of columns that have covariates

- event.name:

  (optional) name of the event (useful for maternal mortality)

- exp.scale:

  defaults to 1/12; see Details

## Value

A `cell_config` object that can be passed into estimation functions to
describe the cells that estimates should be produced for.

## Details

- Note that all of the parameters that require column names are
  expecting strings. These column names refer to the dataset with one
  row for each reported sibling. This isn't passed into this function,
  so this function can't check that these column names are valid.

- `age.groups` can either be the output of one of the helper functions
  for creating age groups (`make.age.groups` or `make.even.age.groups`),
  or it can be '1yr', '5yr', or '10yr' for standard 1, 5, or 10-year age
  groups ranging from 15 to 65, or '1yr_to50', '5yr_to50' for standard 1
  or 5-year age groups ranging from 15 to 49

- `time.periods` can either be the output of `make.time.periods`, or it
  can be '12mo_beforeinterview', '5yr_beforeinterview', or
  '7yr_beforeinterview' for time periods one, five, or seven years
  before the interview date

- `exp.scale` is a factor that is used to convert differences between
  dates into years; this is usually 1/12, since the DHS reports times by
  month (eg Dec 2010 or Sept 1995). Differences in dates are thus
  denominated in months, and need to be multiplied by 1/12 to convert
  them into years.
