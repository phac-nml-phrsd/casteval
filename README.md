
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

See `vignette("casteval")` to get started.

## For developers

### Source code

The source code is located
[here](https://github.com/phac-nml-phrsd/casteval).

### `renv`

This project uses [`renv`](https://rstudio.github.io/renv/index.html) to
manage dependencies.

For R package projects, `renv` tries to intelligently discover
dependencies by looking at the fields in the DESCRIPTION file that are
relevant for users, like `Imports` and `Depends` (see
`renv::settings$package.dependency.fields()`). For developers, the
`Suggests` field can be equally important to track as it can include
packages used in the process of development but not in the actual source
code of the package, like `devtools`, `covr`, to ensure development
environments don’t vary across users.

The `renv` FAQ page [recommends tracking development dependencies in
`Suggests`](https://rstudio.github.io/renv/articles/faq.html#how-should-i-handle-development-dependencies).
It doesn’t, however, seem to tell you that in order to use their
recommendation, you need to use the `dev = TRUE` flag in
`renv::status()` to accurately check the status of such a project, as
well as in `renv::snapshot()` when recording files (the default is
`dev = FALSE`). If you simply execute `renv::snapshot(dev = FALSE)`,
`renv` will miss/remove the development dependencies in `Suggests`,
which will break our CI pipelines. Once these dependencies have been
recorded, `renv::status(dev = FALSE)` will report that the project is
out-of-sync, as it will see installed dependencies that are not used
anywhere (since it skips `Suggests`).

The bottom line is to be sure to use the `dev = TRUE` option when
working in this repository. If you need to add any development
dependencies to the project, be sure to add them to the `Suggests` field
of the `DESCRIPTION`, run `renv::install(dev = TRUE)` to install them,
then `renv::snapshot(dev = TRUE)` to record them.

A side effect of this default behaviour is that, when you open a new R
session in this project, `renv::status()` will get called and report
that the project is out-of-sync. Don’t trust this message! Run
`renv::status(dev = TRUE)` to check the true status of the project.
(There is a [feature
request](https://github.com/rstudio/renv/issues/1760) to enable a user
to configure the default behaviour of `renv::status()` on a
project-by-project basis specifically to accommodate the above
situation, but this issue is still open as of this writing on
2024-06-11…)

### Test coverage

You can use `covr::report()` to periodically check test coverage locally
in development.
