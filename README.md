# learnitprogress - Report Student Progress in LearnIt::R Courses

<!-- badges: start -->

[![R-CMD-check](https://github.com/learnitr/learnitprogress/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/learnitr/learnitprogress/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://img.shields.io/codecov/c/github/learnitr/learnitprogress/main.svg)](https://codecov.io/github/learnitr/learnitprogress?branch=main)
[![CRAN Status](https://www.r-pkg.org/badges/version/learnitprogress)](https://cran.r-project.org/package=learnitprogress)
[![r-universe status](https://learnitr.r-universe.dev/badges/learnitprogress)](https://learnitr.r-universe.dev/learnitprogress)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Lifecycle stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)

<!-- badges: end -->

A Shiny app that connects to you LRS ("Learning Record Store" database, as created and managed by the 'learnitdown' package) and displays a report showing the completion of exercices and projects. This report is intended for students to track their progress in the course.

## Installation

(Not yet! The latest stable version of {learnitprogress} can simply be installed from [CRAN](http://cran.r-project.org):)

``` r
install.packages("learnitprogress")
```

You can also install the latest development version. Make sure you have the {remotes} R package installed:

``` r
install.packages("remotes")
```

Use `install_github()` to install the {learnitprogress} package from GitHub (source from **main** branch will be recompiled on your machine):

``` r
remotes::install_github("SciViews/learnitprogress")
```

R should install all required dependencies automatically, and then it should compile and install {learnitprogress}.

## Further explore {learnitprogress}

You can get further help about this package this way. Make the {learnitprogress} package available in your R session:

``` r
library("learnitprogress")
```

Get help about this package:

``` r
library(help = "learnitprogress")
help("learnitprogress-package")
vignette("learnitprogress") # None is installed with install_github()
```

For further instructions, please, refer to these help pages at <https://learnitr.github.io/learnitprogress/>.

## Code of Conduct

Please note that the {learnitprogress} project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.
