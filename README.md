
# bakenzutil

<!-- badges: start -->
<!-- badges: end -->

The goal of bakenzutil is to provide a collection of utility functions. Probably only useful to bakenzua.
Useful clusters of functions can be carved out to packages....given time!

## Installation

You can install the github hosted version of bakenzutil from [github](https://github.com) with:

``` r
devtools::install_github("bakenzua/bakenzutil")
```

# classifyGasLabels 

classifyGasLabels classifies raw blood gas sample type labels
A vector of the following classifications is returned.

* ARTERIAL
* VENOUS
* MIXED_VENOUS
* PRE_ECMO
* POST_ECMO
* UNKNOWN
