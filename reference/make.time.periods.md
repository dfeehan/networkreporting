# make a time.periods object

make a time.periods object

## Usage

``` r
make.time.periods(start, durations, names)
```

## Arguments

- start:

  the start of the time of interest

- durations:

  the durations of the subsequent time periods

- names:

  the names of the time periods

## Value

an object (list) with the widths, names, and number of time periods as
well as a matrix called template which has the start and end of each
time period. the intervals in template are closed on the left but not on
the right; that is, start of 1900 and end of 1910 means \[1900, 1910) in
terms of exact times.
