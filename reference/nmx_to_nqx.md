# convert age-specific mortality rates to probabilities of dying

The standard life table conversion,

## Usage

``` r
nmx_to_nqx(nmx, n = 5, nax = 2.6)
```

## Arguments

- nmx:

  age-specific mortality rates, as rates per person-year (so divide by
  1,000 first if you have them per 1,000)

- n:

  width of the age interval in years

- nax:

  average years lived in the interval by those who die in it. The
  default of 2.6 is what both the DHS and the MICS tabulation code use;
  pass 2.5 for the textbook value. See Details

## Value

a vector of probabilities, the same length as `nmx`

## Details

\$\${}\_nq_x = \frac{n \cdot {}\_nm_x}{1 + (n - {}\_na_x) \cdot
{}\_nm_x}\$\$

where `nax` is the average number of years lived in the interval by
those who die in it.

## Details

**On the default.** With `n = 5`, `nax = 2.6` makes the denominator
`1 + 2.4 * nmx`, which is what `Chap16_AM/AM_rates.do:1085` and the
MICS6 TM.9 syntax both compute. `AM_rates.do` carries the comment *"See
DHS Guide to Statistics for use of 2.4 rather than 2.5"*, so it is
deliberate on the DHS side; the MICS syntax uses the same value while
its own header documents 2.5.

The difference is small — around 0.2 per 1,000 on a 35q15 of 200 — but
it is free to get right, and published figures are rounded to integers,
so it cannot be recovered by comparing against them.

## Examples

``` r
  # Gambia 2019-20 DHS, women, Table 14.1, rates per 1,000
  nmx <- c(0.93, 1.55, 2.24, 3.27, 3.57, 6.27, 6.25) / 1000
  nmx_to_nqx(nmx)
#> [1] 0.004639644 0.007721277 0.011140111 0.016222684 0.017698360 0.030885239
#> [7] 0.030788177
```
