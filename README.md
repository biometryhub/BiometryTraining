
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BiometryTraining

<!-- badges: start -->

[![Project Status: Active: The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Codecov test
coverage](https://codecov.io/gh/biometryhub/BiometryTraining/branch/master/graph/badge.svg)](https://codecov.io/gh/biometryhub/BiometryTraining?branch=master)
[![R build
status](https://github.com/biometryhub/BiometryTraining/workflows/R-CMD-check/badge.svg)](https://github.com/biometryhub/BiometryTraining/actions)
![pkgdown](https://github.com/biometryhub/BiometryTraining/workflows/pkgdown/badge.svg)
[![minimal R
version](https://img.shields.io/badge/R%3E%3D-3.5.0-6666ff.svg)](https://cran.r-project.org/)
[![packageversion](https://img.shields.io/badge/Package%20version-0.5.2-orange.svg?style=flat-square)](/commits/master)
[![Last-changedate](https://img.shields.io/badge/last%20change-2020--08--26-yellowgreen.svg)](/commits/master)
[![Licence](https://img.shields.io/github/license/mashape/apistatus.svg)](http://choosealicense.com/licenses/mit/)
<!-- badges: end -->

The goal of BiometryTraining is to provide functions to aid in the
Design and Analysis of Agronomic-style experiments through easy access
to documentation and helper functions, especially while teaching these
concepts.

## Installation

We previously recommended installing the package from GitHub, but it is
now easier to use the Biometry Hub [drat
repository](https://biometryhub.github.io/drat/). Use the following code
to install this package:

``` r
if(!require("drat")) install.packages("drat") 
drat::addRepo("biometryhub")
# Install as normal
install.packages("BiometryTraining")
```

### Troubleshooting Installation

  - If you have the latest version already installed, you will get a
    response like:

<span style="color: red;">Skipping install of ‘BiometryTraining’ from a
github remote, the SHA1 (e034b603) has not changed since last
install.<br> Use `force = TRUE` to force installation</span>

  - The following warning messages can usually be ignored:

<span style="color: red;">WARNING: Rtools is required to build R
packages, but is not currently installed.</span>

<span style="color: red;">Please download and install Rtools custom from
<http://cran.r-project.org/bin/windows/Rtools/>.</span>

<span style="color: red;">Warning messages:<br> 1: In untar2(tarfile,
files, list, exdir) :<br> skipping pax global extended headers<br> 2: In
untar2(tarfile, files, list, exdir) :<br> skipping pax global extended
headers</span>

  - If you receive an error that the package could not install because
    `rlang` or another package could not be upgraded, the easiest way to
    deal with this is to uninstall the package(s) that could not be
    updated (`remove.packages("rlang")`). Then reinstall with
    `install.packages("rlang")` and then try installing
    `BiometryTraining` again.

  - If you are trying to install this on R version 3.5.x (or earlier),
    you should note three things:
    
    1.  It *is* possible to install this package on R 3.5.x (and
        possibly earlier versions) as of 4 August 2020, but…
    2.  You should upgrade to a newer version of R as soon as possible,
        as it is no longer tested.
    3.  This package won’t install properly until you have *downgraded*
        a dependency that requires R 3.6+. To do that, use the following
        code:

<!-- end list -->

``` r
remotes::install_version("pbkrtest", version = "0.4-7")
devtools::install_github("biometryhub/BiometryTraining", upgrade = FALSE)
```

## Using the package

Load the package and start using it with:

``` r
library(BiometryTraining)
```
