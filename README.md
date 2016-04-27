[![Travis-CI Build
Status](https://travis-ci.org/dfeehan/networkreporting.svg?branch=master)](https://travis-ci.org/dfeehan/networkreporting)


Summary
================

The goal of the `networkreporting` package is to help people use network reporting methods.

In traditional survey methods, respondents report about themselves.  In network report methods, survey respondents report about the people to whom they are socially connected.  Thus, network reporting methods can be thought of a generalization of traditional surveys.  Many existing methods rely on this kind of indirect reporting and are thus network reporting methods:

- network scale-up method
- sibling method

Although many of the methods that rely on indirect reporting have been regarded as distinct in the past, there is great value in developing a unified statistical framework to deal with common problems that all of these methods share.  This unified framework also enables us to develop new network reporting methods.  This package will help you use both existing network reporting methods and any new network reporting methods that you develop.


For more information on network reporting methods see: Feehan, Dennis (2015) "Network reporting methods", Ph.D. Dissertation. Office of Population Research, Princeton University.

The development of this software was supported by a grant from the National Institutes of Health (R01-HD075666).

Installing
-----------

You can install:

* the latest released version from CRAN with

    ```R
    install.packages("networkreporting")
    ````

* the latest development version from github with

    ```R
    if (packageVersion("devtools") < 1.6) {
      install.packages("devtools")
    }
    devtools::install_github("dfeehan/networkreporting")
    ```

Vignettes
---------
* [Analyzing network scale-up data using the networkreporting package]( https://cran.rstudio.com/web/packages/networkreporting/vignettes/network_scaleup.html)

Branches
--------
* `cran` - will contain the version currently available on
  [CRAN](http://cran.r-project.org)
* `dev` - will have the most recent development release
* other branches will exist as needed


Wish list
---------
* if you would like to suggest a feature, please create an
  [issue](https://github.com/dfeehan/networkreporting/issues)
