[![Travis-CI Build
Status](https://travis-ci.org/dfeehan/networkreporting.svg?branch=master)](https://travis-ci.org/dfeehan/networkreporting)


Summary
================

The goal of the `networkreporting` package is to help people use network reporting methods.

In traditional survey methods, respondents report about themselves.  In network report methods, survey respondents report about the people to whom they are socially connected.  Thus, network reporting methods can be thought of a generalization of traditional surveys.  Many existing methods rely on this kind of indirect reporting and are thus network reporting methods:

- network scale-up method
- sibling method
- multiplicity sampling
- network surivial method
- generalized network scale-up method
- confidant method
- neighborhood method

Although many of the methods that rely on indirect reporting have been regarded as distinct in the past, there is great value in developing a unified statistical framework to deal with common problems that all of these methods share.  This unified framework also enables us to develop new network reporting methods.  This package will help you use both existing network reporting methods and any new network reporting methods that you develop.


For more information on network reporting methods see: 
- Feehan, Dennis (2015) "Network reporting methods", Ph.D. Dissertation. Office of Population Research, Princeton University.

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
    install.packages("devtools")
    devtools::install_github("dfeehan/networkreporting")
    ```

Vignettes
---------

The `networkreporting` package enables you to use several methods that many people currently think of as distinct.  Here are some vignettes for how to use the package:

* [Analyzing network scale-up data using the networkreporting package]( https://cran.r-project.org/web/packages/networkreporting/vignettes/network_scaleup.html)
* Analyzing sibling method data using the networkreporting package (Coming soon)
* Analyzing generalized network scale-up data using the networkreporting package (Coming soon)

Issues
---------
If you would like to suggest a feature or report a bug, please create an [issue](https://github.com/dfeehan/networkreporting/issues)

Citation
-----------

If you use our package for your research, please cite it so that we can continue to develop it.

- Feehan, Dennis M. and Salganik, Matthew J. (2014) "The networkreporting package." http://cran.r-project.org/package=networkreporting
