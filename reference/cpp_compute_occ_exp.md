# cpp_compute_occ_exp

compute occurrences and exposures from lifelines, age groups, and time
intervals

## Usage

``` r
cpp_compute_occ_exp(lambda, alpha, alpha_offset, tau)
```

## Arguments

- lambda:

  a matrix of lifelines whose rows are units of observation
  (individuals), and whose columns are (in order): start time, end time,
  event time

- alpha:

  a matrix whose rows are age groups and whose columns are (in order):
  start time, end time (both starting from 0)

- alpha_offset:

  a vector with the birthdate of each unit of observation or, more
  generally, the offset to use for the age groups in alpha

- tau:

  a matrix of time periods whose rows are units of observation
  (individuals), and whose columns are (in order): start time, end time

## Value

a list containing two matrices, 'occ' and 'exp. each matrix has one row
for each unit of observation (individua) whose columns are (for exp, in
order): age group 1 exposure, ..., last age group exposure; and (for
occ, in order) age group 1 number of events, ..., last age group number
of events

## Details

TODO - should write a more detailed description
