`%notin%` <- `%!in%` <- Negate(`%in%`)

# quiet
#' Function to suppress output if desired, especially useful for ASreml output
#'
#' @param x A function call with output to be suppressed.
#'
#' @return The invisible output of the function called.
#'
#' @keywords internal
#'
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}



######################################################
# Start up function
# this function is executed once the package is loaded
######################################################
.onAttach = function(library, pkg)
{
    # Rv = R.Version()
    # if(!exists("getRversion", baseenv()) || (getRversion() < "2.1"))
    #     stop("This package requires R 2.1 or later")
    # assign(".sommer.home", file.path(library, pkg),
    #        pos=match("package:sommer", search()))
    # sommer.version = "4.1.2 (2021-01-01)" # usually 2 months before it expires

    ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    ### check which version is more recent
    #yyy <- 1.8
    #chooseCRANmirror(ind=114)
    #xxx <- available.packages(contriburl = contrib.url(repos="http://mirror.las.iastate.edu/CRAN/", type = getOption("pkgType")))

    #xxx <- available.packages()
    #current <- as.numeric(xxx["sommer","Version"])
    ### final check
    ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    # assign(".sommer.version", sommer.version, pos=match("package:sommer", search()))
    # if(interactive())
    # {
    #     packageStartupMessage(magenta(paste("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")),appendLF=TRUE)
    #     packageStartupMessage(magenta(paste("~~  Biometry Training ", sommer.version, " []",sep="")),appendLF=TRUE)
    #     packageStartupMessage(magenta(paste("~~   ------------- Multivariate Linear Mixed Models --------------  []")),appendLF=TRUE)
    #     packageStartupMessage(magenta("[]   Author: Giovanny Covarrubias-Pazaran                           []"),appendLF=TRUE)
    #     packageStartupMessage(magenta("[]   Published: PLoS ONE 2016, 11(6):1-15                           []"),appendLF=TRUE)
    #     packageStartupMessage(magenta("[]   Dedicated to the University of Chapingo and UW-Madison         []"),appendLF=TRUE)
    #     packageStartupMessage(magenta("[]   Type 'vignette('v1.sommer.quick.start')' for a short tutorial  []"),appendLF=TRUE)
    #     packageStartupMessage(magenta("[]   Type 'citation('BiometryTraining')' to know how to cite this package           []"),appendLF=TRUE)
    #     packageStartupMessage(magenta(paste("[]==================================================================[]")),appendLF=TRUE)
    #     packageStartupMessage(magenta("sommer is updated on CRAN every 4-months due to CRAN policies"),appendLF=TRUE)
    #     packageStartupMessage(magenta("Newest source is available at https://github.com/covaruber/sommer"),appendLF=TRUE)
    #     packageStartupMessage(magenta("To install type: library(devtools); install_github('covaruber/sommer')"),appendLF=TRUE)
    #
    #     #if(yyy > current){ # yyy < current in CRAN
    #     #  packageStartupMessage(paste("Version",current,"is now available."),appendLF=TRUE) # version current
    #     #  packageStartupMessage(paste("Please update 'sommer' installing the new version."),appendLF=TRUE) # version current
    #     #}
    # }
    invisible()
}
