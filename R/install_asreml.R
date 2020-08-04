#' Install ASreml-R package
#'
#' @description A helper function for installing the ASreml-R package, intended to reduce the difficulty of finding the correct version for your operating system and R version.
#'
#' @param library Library location to install ASreml-R. Uses first option in `.libPaths()` by default.
#' @param quiet Logical (default `FALSE`). Should package be installed quietly?
#' @param force Logical (default `FALSE`). Force ASreml-R to install. Useful for upgrading if it is already installed.
#' @param keep_file Should the downloaded asreml package file be kept? Default is `FALSE`. `TRUE` downloads to current directory. Can also provide a file path to save to another directory.
#'
#' @importFrom utils installed.packages install.packages download.file remove.packages
#' @importFrom httr GET write_disk progress
#'
#' @export
#'
#' @return Silently returns `TRUE` if `asreml` installed successfully or already present, `FALSE` otherwise. Optionally prints a confirmation message on success.
#'
install_asreml <- function(library = .libPaths()[1], quiet = FALSE, force = FALSE, keep_file = FALSE) {
  if("asreml" %in% installed.packages()[,1] & !force) {
    if(!quiet) message("ASreml-R is already installed.")
    invisible(TRUE)
  }
  else {
    if(!quiet) {
      message("\nDownloading and installing ASreml-R. This may take some time, depending on internet speed...\n")
    }
    if(force & "asreml" %in% (.packages())) {
      detach("package:asreml", unload = TRUE)
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

    # Cases:
    # File exists temp
    #   Keep file -> move to dir if true, path if char, nothing if false
    # File exists dir
    #   Keep file -> move to path if char, otherwise nothing
    # File doesn't exist
    #   Download to temp, rename. copy to dir if keep = T, path if keep = path, rename otherwise

    # if(!force) {
    # first check if file already exists, both in the current directory and temp folder
    temp_files <- list.files(tempdir())
    dir_files <- list.files()
    check_temp <- startsWith(temp_files, prefix = "asreml")
    check_dir <- startsWith(dir_files, prefix = "asreml")

    if(any(check_temp)) {
      filename <- temp_files[which(check_temp)]
      save_file <- paste0(tempdir(), "/", filename)

      if(keep_file == TRUE) {
        install_file <- filename
        file.copy(save_file, filename)
      }

      else if(is.character(keep_file)) {
        if(!dir.exists(keep_file)) {
          stop("Directory provided in keep_path does not exist. Please provide a path in the keep_file argument to save the package to.")
        }
        else {
          install_file <- paste0(keep_file, "/", filename)
          file.copy(save_file, install_file)
        }
      }
      else {
        install_file <- save_file
      }
    }
    else if(any(startsWith(dir_files, prefix = "asreml"))) {
      filename <- dir_files[which(check_dir)]
      save_file <- filename

      if(is.character(keep_file)) {
        if(!dir.exists(keep_file)) {
          stop("Directory provided in keep_path does not exist. Please provide a path in the keep_file argument to save the package to.")
        }
        else {
          # Check if there's a trailing slash or not
            install_file <- paste0(keep_file,
                                   ifelse(substr(keep_file, nchar(keep_file), nchar(keep_file)) == "/", "", "/"),
                                   filename)
          file.copy(save_file, install_file)
          file.remove(save_file)
        }
      }
      else {
        install_file <- filename
      }
    }

    else {
      #Create a temporary file to save the package
      save_file <- tempfile("asreml_")

      # Use httr to GET the file which also gives the expanded URL
      response <- httr::GET(url = url, httr::write_disk(save_file), if(!quiet){httr::progress()})

      # Find position of the last / in the expanded URL
      pos <- regexpr("\\/[^\\/]*$", response$url)

      # Extract everything after the last / as the filename
      filename <- substr(response$url, pos+1, nchar(response$url))

      # If keep_file is true, copy asreml to current directory
      if(keep_file == TRUE) {
        install_file <- filename
        file.copy(save_file, filename)
      }
      else if(!keep_file) {
        install_file <- paste0(tempdir(), "/", filename)
        file.rename(save_file, install_file)
      }
      else { # Assume keep_file is a path
        if(!dir.exists(keep_file)) {
          stop("Directory provided in keep_path does not exist. Please provide a valid path in the keep_file argument to save the package to.")
        }
        else {
          install_file <- paste0(keep_file, "/", filename)
          file.copy(save_file, install_file)
        }
      }
    }

    # If forcing installation, remove old version to avoid errors on installation
    if("asreml" %in% installed.packages()[,1] & force) {
      suppressMessages(remove.packages("asreml"))
    }

    # Install asreml
    install.packages(install_file, repos = NULL, quiet = quiet)

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

#' Update asreml package
#'
#' @param ... other arguments passed to [BiometryTraining::install_asreml()]
#'
#' @export
update_asreml <- function(...) {
  install_asreml(force = T, ...)
}
