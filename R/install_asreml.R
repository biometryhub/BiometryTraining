#' Install ASreml-R package
#'
#' @description A helper function for installing the ASreml-R package, intended to reduce the difficulty of finding the correct version for your operating system and R version.
#'
#' @param library Library location to install ASreml-R. Uses first option in `.libPaths()` by default.
#' @param quiet Should package be installed quietly? Default is `TRUE`.
#' @param force Force ASreml-R to install, ignoring if it is already installed.
#'
#' @importFrom utils installed.packages install.packages download.file
#' @importFrom httr GET write_disk progress
#'
#' @return Silently returns `TRUE` if `asreml` installed successfully or already present, `FALSE` otherwise. Also prints a confirmation message on success.
#'
#' @keywords internal
#'
install_asreml <- function(library = .libPaths()[1], quiet = FALSE, force = FALSE) {
    if("asreml" %in% installed.packages()[,1] & !force) {
        if(!quiet) message("ASreml-R is already installed.")
        invisible(TRUE)
    }
    else {
        if(!quiet) {
            message("\nDownloading and installing ASreml-R. This may take some time, depending on internet speed...\n")
        }
        if(force & require(asreml, quietly = T)) {
            detach(package:asreml, unload = T)
        }

        os <- switch(Sys.info()[['sysname']],
                     Windows = "win",
                     Linux   = "linux",
                     Darwin  = "mac"
        )

        ver <- paste(os, substr(getRversion(), 1, 3), sep = "_")

        url <- switch(ver,
                      win_3.5 = {"https://link.biometryhubwaite.com/win-35"},
                      win_3.6 = {"https://link.biometryhubwaite.com/win-36"},
                      win_4.0 = {"https://link.biometryhubwaite.com/win-40"},
                      win_4.1 = {"https://link.biometryhubwaite.com/win-41"},
                      mac_3.5 = {"https://link.biometryhubwaite.com/mac-35"},
                      mac_3.6 = {"https://link.biometryhubwaite.com/mac-36"},
                      mac_4.0 = {"https://link.biometryhubwaite.com/mac-40"},
                      linux_3.5 = {"https://link.biometryhubwaite.com/linux-35"},
                      linux_3.6 = {"https://link.biometryhubwaite.com/linux-36"},
                      linux_4.0 = {"https://link.biometryhubwaite.com/linux-40"}
        )

        #Create a temporary file to save the package
        tmp_file <- tempfile("Asreml_")

        # Use httr to GET the file which also gives the expanded URL
        response <- httr::GET(url, httr::write_disk(tmp_file), if(!quiet){httr::progress()})

        # Find position of the last / in the expanded URL
        pos <- regexpr("\\/[^\\/]*$", response$url)

        # Extract everything after the last / as the filename
        filename <- substr(response$url, pos+1, nchar(response$url))
        new_file <- paste0(tempdir(), ifelse(os == "win", "\\", "/"), filename)
        file.rename(tmp_file, new_file)

        # Download the file
        # pb <- progress::progress_bar$new(format = "Downloading ASreml-R: [:bar] :percent eta: :eta",
        #                                 total = 100, clear = FALSE, width= 60)
        # pb$tick()
        # download.file(url, destfile = save_path, quiet = quiet)

        # Install asreml
        install.packages(new_file, lib = library, repos = NULL, quiet = quiet)

        if("asreml" %in% installed.packages()[,1]) {
            if(!quiet) message("ASreml-R successfully installed!")
        }
        else {
            if(!quiet) warning("There was a problem with installation and ASreml-R was not successfully installed.")
            invisible(FALSE)
        }
        invisible(TRUE)
    }
}

update_asreml <- function(...) {
  install_asreml(..., force=T)
}
