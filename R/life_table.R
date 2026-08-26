## life_table.R -- converting age-specific mortality rates into probabilities of
## dying, the way DHS and MICS reports do it.
##
## The constant matters more than it looks. Both reference implementations use
## 2.4 as the coefficient, i.e. nax = 2.6, not the textbook 2.5:
##
##   DHS   Chap16_AM/AM_rates.do:1084
##           //See DHS Guide to Statistics for use of 2.4 rather than 2.5
##           gen q5=5*mx/(1+2.4*mx)
##   MICS  "MICS6 - 06 - TM.9.1&TM.9.2&TM.9.3&DQ.7.1&DQ7.2.sps"
##           compute fqx = 1 - ( 5 * (adfmr/1000) / (1+2.4*(adfmr/1000)) ).
##
## DHS says the Guide documents the choice deliberately; the MICS syntax
## contradicts its own header, which states nax = 2.5. They agree in the code.

##' convert age-specific mortality rates to probabilities of dying
##'
##' The standard life table conversion,
##'
##' \deqn{{}_nq_x = \frac{n \cdot {}_nm_x}{1 + (n - {}_na_x) \cdot {}_nm_x}}
##'
##' where `nax` is the average number of years lived in the interval by those
##' who die in it.
##'
##' @param nmx age-specific mortality rates, as rates per person-year (so
##'        divide by 1,000 first if you have them per 1,000)
##' @param n width of the age interval in years
##' @param nax average years lived in the interval by those who die in it. The
##'        default of 2.6 is what both the DHS and the MICS tabulation code use;
##'        pass 2.5 for the textbook value. See Details
##' @return a vector of probabilities, the same length as `nmx`
##' @section Details:
##'
##' **On the default.** With `n = 5`, `nax = 2.6` makes the denominator
##' `1 + 2.4 * nmx`, which is what `Chap16_AM/AM_rates.do:1085` and the MICS6
##' TM.9 syntax both compute. `AM_rates.do` carries the comment *"See DHS Guide
##' to Statistics for use of 2.4 rather than 2.5"*, so it is deliberate on the
##' DHS side; the MICS syntax uses the same value while its own header documents
##' 2.5.
##'
##' The difference is small --- around 0.2 per 1,000 on a 35q15 of 200 --- but it
##' is free to get right, and published figures are rounded to integers, so it
##' cannot be recovered by comparing against them.
##'
##' @examples
##'   # Gambia 2019-20 DHS, women, Table 14.1, rates per 1,000
##'   nmx <- c(0.93, 1.55, 2.24, 3.27, 3.57, 6.27, 6.25) / 1000
##'   nmx_to_nqx(nmx)
##' @export
##' @md
nmx_to_nqx <- function(nmx, n = 5, nax = 2.6) {

  if (nax >= n) {
    stop(glue::glue(
      "`nax` ({nax}) must be less than the interval width `n` ({n}): it is the ",
      "average number of years lived *within* the interval by those who die in it."))
  }

  (n * nmx) / (1 + (n - nax) * nmx)
}


##' probability of dying between exact ages 15 and 50
##'
##' Converts seven five-year age-specific mortality rates into
##' \eqn{{}_{35}q_{15}}, the probability that someone alive at exact age 15 dies
##' before exact age 50, under the synthetic-cohort assumption that these rates
##' hold constant.
##'
##' @param nmx seven age-specific mortality rates for the groups 15--19 through
##'        45--49, in the order returned by
##'        \code{siblingsurvival::reproductive_age_groups()}, as rates per person-year
##' @param n width of each age interval in years
##' @param nax see [nmx_to_nqx]; the default matches the DHS and MICS
##'        implementations
##' @param per express the result per this many people. `1000` is the
##'        convention in both DHS and MICS reports
##' @return a single number
##' @section Details:
##'
##' \deqn{{}_{35}q_{15} = 1 - \prod_{x} (1 - {}_5q_x)}
##'
##' `nmx` must be *rates*, not counts, and must not already be multiplied by
##' 1,000 --- a common way to get an answer that is wrong by orders of magnitude.
##'
##' @examples
##'   # Gambia 2019-20 DHS, Table 14.1 rates per 1,000; report gives 114 and 124
##'   women <- c(0.93, 1.55, 2.24, 3.27, 3.57, 6.27, 6.25) / 1000
##'   men   <- c(1.44, 2.30, 2.29, 3.40, 4.91, 6.11, 6.11) / 1000
##'   q15_to_50(women)
##'   q15_to_50(men)
##' @export
##' @md
q15_to_50 <- function(nmx, n = 5, nax = 2.6, per = 1000) {

  if (length(nmx) != 7) {
    stop(glue::glue(
      "expected 7 age-specific rates (15-19 through 45-49), got {length(nmx)}."))
  }

  if (any(!is.na(nmx) & nmx > 1)) {
    warning(paste0(
      "some rates are greater than 1, which usually means they are per 1,000 ",
      "rather than per person-year. Divide by 1000 first."))
  }

  per * (1 - prod(1 - nmx_to_nqx(nmx, n = n, nax = nax)))
}
