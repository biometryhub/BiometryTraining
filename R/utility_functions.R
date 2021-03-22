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
# .onAttach <- function(library, pkg)
# {
#     current_version <- "0.7.0"
#     installed_version <- packageVersion('BiometryTraining')
#
#     # check which version is more recent
#     # xxx <- available.packages(type = getOption("pkgType")))
#
#     #xxx <- available.packages()
#     #current <- as.numeric(xxx["BiometryTraining","Version"])
#     ### final check
#     ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#     # assign(".sommer.version", sommer.version, pos=match("package:sommer", search()))
#     if(interactive())
#     {
#         packageStartupMessage("    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~", appendLF=TRUE)
#         packageStartupMessage(paste("    |  Biometry Training ", installed_version, "                                         |",sep=""),appendLF=TRUE)
#         packageStartupMessage("    |  Authors: Sam Rogers, Sharon Nielsen, Annie Conway               |",appendLF=TRUE)
#         packageStartupMessage("    |  Developed at the University of Adelaide with funding provided   |",appendLF=TRUE)
#         packageStartupMessage("    |  by the Grains Research and Development Corporation              |",appendLF=TRUE)
#         packageStartupMessage("    |                                                                  |",appendLF=TRUE)
#         packageStartupMessage("    |  If you use this package, please cite it! Type                   |",appendLF=TRUE)
#         packageStartupMessage("    |  'citation('BiometryTraining')' to see how to cite this package  |",appendLF=TRUE)
#         packageStartupMessage("    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~",appendLF=TRUE)
#         # packageStartupMessage("sommer is updated on CRAN every 4-months due to CRAN policies",appendLF=TRUE)
#         packageStartupMessage("    The latest version of this package is available at", appendLF=TRUE)
#         packageStartupMessage("    https://github.com/biometryhub/BiometryTraining. To install type:", appendLF=TRUE)
#         packageStartupMessage("    library(remotes); install_github('biometryhub/BiometryTraining')",appendLF=TRUE)
#
#         #if(yyy > current){ # yyy < current in CRAN
#         #  packageStartupMessage(paste("Version",current,"is now available."),appendLF=TRUE) # version current
#         #  packageStartupMessage(paste("Please update 'sommer' installing the new version."),appendLF=TRUE) # version current
#         #}
#     }
#     invisible()
# }
