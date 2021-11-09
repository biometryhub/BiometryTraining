# BiometryTraining 0.9.1

## Minor changes

- Enabled model call to be displayed on resplots for easy comparison between several models (#81)

# BiometryTraining 0.9.0

## Major changes

- `resplot()` function now supports models from the packages `lme4`, `nlme`, and `sommer`.
- `mct.out()` function now supports models from the packages `lme4` and `nlme`. Support for `sommer` models is in progress.

## Minor changes

- Fixed some bugs in `logl.test()`.
- Unknown arguments in `design()` are no longer silently ignored (#77)
- Enabled a way for split plot designs to be arranged row-wise or column-wise (#68)
- Added split plot treatment labels to the output plot (#78)
- Prevented misspelled arguments passed in the ellipsis from being silently ignored (#77)

# BiometryTraining 0.8.0

## Minor changes

- Updates to the documentation of `mct.out()`.
- Fixed `resplot()` so that plot C is now easier to interpret because it's not as stretched (#72)
- Switched from `patchwork` to `cowplot` (#74)
- Reduced default height of labels in `mct.out()` plot (#67)

## Bug fixes

- Fixed a bug where factorial latin square designs were not generated properly (#62)
- Updated labels on design outputs so that they are consistent and prevent 3 way factorials (#65)
- Fixed up labelling in split plot designs (#64)
- Fixed an issue where incorrect significance values in `mct.out()` give a negative confidence interval (#73)
- Fixed a bug where crossed designs didn't produce spreadsheet output (#66)

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
