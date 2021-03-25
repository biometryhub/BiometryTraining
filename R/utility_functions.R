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
.onAttach <- function(library, pkg)
{
  # current_version <- "0.7.0"
  installed_version <- packageVersion('BiometryTraining')

  if(interactive())
  {
    output <- paste("    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~",
    paste("    |  Biometry Training version ", installed_version, "                                  |",sep=""),
    "    |  Authors: Sharon Nielsen, Sam Rogers, Annie Conway                |",
    "    |  Developed at the University of Adelaide with funding provided    |",
    "    |  by the Australian Grains Research and Development Corporation.   |",
    "    |  Package website: https://biometryhub.github.io/BiometryTraining  |",
    "    |                                                                   |",
    "    |  If you have used this package in your work, please cite it.      |",
    "    |  Type 'citation('BiometryTraining')' for the citation details.    |",
    "    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n", sep = "\n")

    if(requireNamespace("crayon", quietly = TRUE)) {
      packageStartupMessage(crayon::green(output), appendLF=TRUE)
    }
    else {
      packageStartupMessage(output, appendLF=TRUE)
    }

    # check which version is more recent
    current_version <- tryCatch(
      {
        packages <- available.packages()
        ver <- packages["BiometryTraining","Version"]
      },
      error=function(cond) {
        NA
      }
    )

    if(!is.na(current_version) && current_version > installed_version){ # installed version < current version on CRAN
      warning("    BiometryTraining version ", current_version, " is now available.\n",
              "    Please update BiometryTraining by running\n",
              "    install.packages('BiometryTraining')", call. = F)
    }
    else {
      output2 <- paste("    The latest version of this package is available at",
      "    https://github.com/biometryhub/BiometryTraining. To install type:",
      "    remotes::install_github('biometryhub/BiometryTraining')", sep = "\n")

      if(requireNamespace("crayon", quietly = TRUE)) {
        packageStartupMessage(crayon::green(output2),appendLF=TRUE)
      }
      else {
        packageStartupMessage(output2,appendLF=TRUE)
      }
    }
  }
  invisible()
}
