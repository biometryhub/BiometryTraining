# BiometryTraining 1.0.0

First release for CRAN.

## Minor changes

- Updates to the documentation of `mct.out()`.

# BiometryTraining 0.7.1

## Bug fixes

- Fixed a bug based on an old version of `patchwork` by requiring `patchwork` version 1.1.0 or later.

# BiometryTraining 0.7.0

* Added a `NEWS.md` file to track changes to the package.

## Major changes

- Introduced the `design()` function to create designs and produce the output in one step. (#40)
- `resplt()` now automatically produces multiple residual plots in situations like multisite analyses (#53)
- Updated the look of `resplt()` and included output of the Shapiro-Wilk's test for normality.
- Bumped required minimum version of R to 3.6.0.

## Minor changes

- Added the alias `resplot()` which is identical to `resplt()` but may be easier to spell.
- Organised the functions on the website under major headings.

## Bug fixes

- Fixed a bug where aliased treatment levels would cause `mct.out()` to crash (#47)
- Fixed a bug where `logltest()` could infinitely cycle with models that don't converge (#50)
- Fixed a bug where `logltest()` would print all the terms on the boundary.
- Reduced the amount of warnings and messages printed by `logltest()` (#17)
