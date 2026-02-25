# handle the total.popn.size argument in a uniform way across several functions

handle the total.popn.size argument in a uniform way across several
functions, including
[`kp.degree.estimator`](http://dennisfeehan.org/networkreporting/reference/kp.degree.estimator.md),
[`nsum.internal.consistency`](http://dennisfeehan.org/networkreporting/reference/nsum.internal.consistency.md),
and
[`nsum.estimator`](http://dennisfeehan.org/networkreporting/reference/nsum.estimator.md).

## Usage

``` r
parse.total.popn.size(total.popn.size, survey.data, verbose = FALSE)
```

## Arguments

- total.popn.size:

  value to parse

- survey.data:

  the dataframe we're analyzing, which may or may not have an attribute
  called 'total.popn.size'

- verbose:

  if TRUE, print messages to the screen

## Value

the parsed total population size

## Details

The result depends upon the value that was passed in:

- NA if total.popn.size is NA then work with proportions

- NULL if total.popn.size is NULL (nothing passed in), then assume that
  there's a total.popn.size attribute associated with the dataset we're
  using

- numerical value if an actual total.popn.size was passed in,
