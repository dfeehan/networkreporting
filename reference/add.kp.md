# attach known populations to a dataframe

take a known population vector (see
[`df.to.kpvec`](http://dennisfeehan.org/networkreporting/reference/df.to.kpvec.md))
and associate it with a survey dataframe. this makes it more convenient
to use some of the `networksampling` package's functions

## Usage

``` r
add.kp(survey.data, kp.vec, total.popn.size = NULL)
```

## Arguments

- survey.data:

  the survey dataframe

- kp.vec:

  the known population vector

- total.popn.size:

  (optional) the total population size to use (see below)

## Value

the survey dataframe with the known population vector attached as an
attribute

## Details

The `total.popn.size` parameter is interpreted as follows:

- NA if total.popn.size is NA then work with proportions

- NULL if total.popn.size is NULL (nothing passed in), then assume that
  there's a total.popn.size attribute associated with the dataset we're
  using

- numerical value if an actual total.popn.size was passed in, use that
  value

## See also

[df.to.kpvec](http://dennisfeehan.org/networkreporting/reference/df.to.kpvec.md)

## Examples

``` r
if (FALSE) { # \dontrun{

  # if kp.dat is a dataframe with columns 'kp' with known popn names
  # and 'total.size' with the total size,
  # and my.survey is the dataframe with survey responses

  my.kp.vec <- df.to.kpvec(kp.data, kp.var='kp', kp.value='total.size')
  my.survey <- add.kp(my.survey, my.kp.vec)

  # now we can call estimator functions like
  # kp.degree.estimator without having to specify known
  # populations each time
} # }
```
