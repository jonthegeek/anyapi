
<!-- README.md is generated from README.Rmd. Please edit that file -->

# anyapi

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/anyapi)](https://CRAN.R-project.org/package=anyapi)
[![Codecov test
coverage](https://codecov.io/gh/jonthegeek/anyapi/branch/main/graph/badge.svg)](https://app.codecov.io/gh/jonthegeek/anyapi?branch=main)
[![R-CMD-check](https://github.com/jonthegeek/anyapi/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jonthegeek/anyapi/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Tools to interactively query hundreds of web ‘APIs’, with documentation.

## Installation

You can install the development version of anyapi from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("jonthegeek/anyapi")
```

## Usage

Find an API. Such APIs can be used with {beekeeper} to create an API
wrapper package.

``` r
library(anyapi)
find_api("trello")
#> # A tibble: 1 × 4
#>   title  provider   service apid_url                                            
#>   <chr>  <chr>      <chr>   <chr>                                               
#> 1 Trello trello.com <NA>    https://api.apis.guru/v2/specs/trello.com/1.0/opena…
```

## Code of Conduct

Please note that the anyapi project is released with a [Contributor Code
of Conduct](https://jonthegeek.github.io/anyapi/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
