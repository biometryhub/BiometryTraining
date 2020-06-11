#' Install Asreml package
#'
#' @param library Library location to install ASReml-R. Uses default library location by default.
#' @param quiet Should package be installed quietly? Default is `TRUE`.
#'
#' @importFrom curl curl_download
#' @importFrom utils installed.packages
#'
#' @return `TRUE` if asreml installed successfully or already present, `FALSE` otherwise. Also prints a confirmation message on success.
#' @export
#'
#' @keywords internal
#'
install_asreml <- function(library = .libPaths()[1], quiet = TRUE) {
    if("asreml" %in% installed.packages()[,1]) {
        return(TRUE)
    }
    else {
        opts <- c("", "mac_3.5", "linux_3.5", "win_3.6", "mac_3.6", "linux_3.6",
                  "win_4.0", "mac_4.0", "linux_4.0")

        os <- switch(Sys.info()[['sysname']],
                     Windows = "win",
                     Linux   = "linux",
                     Darwin  = "mac"
        )

        ver <- paste(os, substr(getRversion(), 1, 3), sep = "_")

        link <- switch(ver,
                       win_3.5 = {}
        )

        # curl::curl_download()

        return(ver)


        # Download and install appropriate version
    }
}
