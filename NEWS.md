# BiometryTraining 0.7.0

* Added a `NEWS.md` file to track changes to the package.

## Major changes

- Introduced the `design()` function to create designs and produce the output in one step. (#40)
- `resplt()` now automatically produces multiple residual plots in situations like multisite analyses (#53)
- Updated the look of `resplt()` and included output of the Shapiro-Wilk's test for normality.
- Bumped required version of R to 3.6.0+.

## Minor changes

- Added the alias `resplot()` which is identical to `resplt()`

## Bug fixes

- Fixed a bug where aliased treatment levels would cause `mct.out` to crash (#47)
- Fixed a bug where logltest could infinitely cycle with models that don't converge (#50)
