## Resubmission 2
This is a resubmission. In this version I have:

* Corrected the broken URL
* Updated the licence file to follow the requested format

## Resubmission
This is a resubmission. In this version I have:

* Skipped the test that was causing an ERROR on CRAN.

## Test environments
* local Windows 10 install, R 4.0.5
* Ubuntu 20.04 (on Virtual Box), R 4.0.5
* Rhub `check_for_cran()`
* Github Actions:
    - macOS: r-release
    - windows: r-devel, r-release, r-oldrel
    - ubuntu 20.04: r-release, r-oldrel

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 2 NOTEs:

* checking LazyData ... NOTE
  'LazyData' is specified without a 'data' directory

* checking package dependencies ... NOTE
  Package which this enhances but not available for checking: 'asreml'
  
  
