
<!-- README.md is generated from README.Rmd. Please edit that file and re-build the .md with devtools::build_readme() -->

# casteval

<!-- badges: start -->
<!-- badges: end -->

## Overview

`{casteval}` facilitates the evaluation of time series forecasts.

✅ Flexibly accepts forecasts as either a set of individual model
realizations or as the summary of an ensemble of realizations via
quantiles at each time point

✅ Includes multiple scoring methods

✅ Provides a suite of visualization tools to help assess forecast and
scoring performance

## Installation

The simplest way to install `{casteval}` is to use the
[`remotes`](https://remotes.r-lib.org/) package:

``` r
# install.packages("remotes")
remotes::install_github("phac-nml-phrsd/casteval")
```

If you do not wish to install the `remotes` package, or have trouble
using it, you can simply clone this repository locally and then use:

``` r
install.packages("your/path/to/casteval", type="source")
```

## Usage

See `vignette("casteval")` to get started with using this package.

## For developers

Please review [the developer
guide](https://phac-nml-phrsd.github.io/casteval/articles/dev.html) if
you plan on contributing to this package.
