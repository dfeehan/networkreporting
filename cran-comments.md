
## Test environments

* local OS X install, R 3.3.0 RC (2016-04-28 r70564)
* ubuntu 12.04 (on travis-ci), R 3.2.5

## R CMD CHECK results

There were no ERRORs and no WARNINGs.

There was 1 NOTE:

    checking dependencies in R code ... NOTE
    Unexported objects imported by ':::' calls:
      ‘surveybootstrap:::get.var’ ‘surveybootstrap:::get.weights’
      ‘surveybootstrap:::vcat’
      See the note in ?`:::` about the use of this operator.

I am also the maintainer of the surveybootstrap package, and didn't
want to have duplicate code in both places. My understanding is that
this is acceptable (since I maintain both packages).
But if it is not, please let me know what I should do to resolve this.

## Downstream dependencies

None.



