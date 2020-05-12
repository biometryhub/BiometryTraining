
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
[![packageversion](https://img.shields.io/badge/Package%20version-0.3.1-orange.svg?style=flat-square)](/commits/master)
[![Last-changedate](https://img.shields.io/badge/last%20change-2020--05--12-yellowgreen.svg)](/commits/master)
[![Licence](https://img.shields.io/github/license/mashape/apistatus.svg)](http://choosealicense.com/licenses/mit/)
<!-- badges: end -->

The goal of BiometryTraining is to provide functions to aid in the
Design and Analysis of Agronomic-style experiments through easy access
to documentation and helper functions, especially while teaching these
concepts.

## Installation

You can install the latest version of BiometryTraining from
[GitHub](https://github.com/) with:

``` r
if(!require("devtools")) install.packages("devtools") 
devtools::install_github("biometryhub/BiometryTraining", upgrade = FALSE)
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

## Using the package

Load the package and start using it with:

``` r
library(BiometryTraining)
```
