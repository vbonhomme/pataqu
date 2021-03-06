
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pataqu [<img src="man/figures/pataqu.png" width = "175" height = "200" align="right" />]()

<!-- badges: start -->
<!--[![CRAN status](https://www.r-pkg.org/badges/version/pataqu)](https://CRAN.R-project.org/package=pataqu)-->

[![R-CMD-check](https://github.com/vbonhomme/pataqu/workflows/R-CMD-check/badge.svg)](https://github.com/vbonhomme/pataqu/actions)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![](https://img.shields.io/github/last-commit/vbonhomme/pataqu.svg)](https://github.com/vbonhomme/pataqu/commits/master)
[![Codecov test
coverage](https://codecov.io/gh/vbonhomme/pataqu/branch/master/graph/badge.svg)](https://app.codecov.io/gh/vbonhomme/pataqu?branch=master)

<!-- badges: end -->

**pataqu** uses permutationnal approaches to deal with data that comes
with uncertainties on x.

Its approach originated when working with temporal data that are not
exactly defined but comes with lower (and upper) bounds, also called
[terminus ante (and post)
quem](https://en.wikipedia.org/wiki/Terminus_post_quem).

pataqu can also be used when dating comes with a confidence interval
likely to affect interpretation, for instance [radiocarbon
dating](https://en.wikipedia.org/wiki/Radiocarbon_dating),

More generally, it allows to inspect both graphically and statistically
the effects of x uncertainties.

## Installation

You can install the development version of pataqu from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("vbonhomme/pataqu")
```

Or the latest version released on CRAN with:

``` r
install.packages("pataqu")
```
