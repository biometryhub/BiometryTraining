#' Install Asreml package
#'
#' @param quiet Should package be installed quietyly? Default is `TRUE`.
#'
#' @return
#' @export
#'
#' @keywords internal
#'
install_asreml <- function(quiet = TRUE) {

    opts <- c("win_3.5", "mac_3.5", "linux_3.5", "win_3.6", "mac_3.6", "linux_3.6",
              "win_4", "mac_4", "linux_4")

    # Check OS

    switch(Sys.info()[['sysname']],
           Windows= "win",
           Linux  = "linux",
           Darwin = "mac"
    )






    # Check R version

    # Download and install appropriate version
}
