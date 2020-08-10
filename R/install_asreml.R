#' Install ASreml-R package
#'
#' @description A helper function for installing the ASreml-R package, intended to reduce the difficulty of finding the correct version for your operating system and R version.
#' @param library Library location to install ASReml-R. Uses first option in `.libPaths()` by default.
#' @param quiet Should package be installed quietly? Default is `TRUE`.
#'
#' @importFrom utils installed.packages install.packages download.file
#'
#' @return Silently returns `TRUE` if asreml installed successfully or already present, `FALSE` otherwise. Also prints a confirmation message on success.
#' @export
#'
#' @keywords internal
#'
install_asreml <- function(library = .libPaths()[1], quiet = FALSE) {
    if("asreml" %in% installed.packages()[,1]) {
        if(!quiet) message("ASreml-R already installed!")
        invisible(TRUE)
    }
    else {
        if(!quiet) {
            message("\nDownloading and installing ASreml-R. This may take some time, depending on internet speed...\n")
        }
        opts <- c("", "mac_3.5", "linux_3.5", "win_3.6", "mac_3.6", "linux_3.6",
                  "win_4.0", "mac_4.0", "linux_4.0")

        os <- switch(Sys.info()[['sysname']],
                     Windows = "win",
                     Linux   = "linux",
                     Darwin  = "mac"
        )

        ver <- paste(os, substr(getRversion(), 1, 3), sep = "_")

        url <- switch(ver,
                      win_3.5 = {"https://downloads.vsni.digital/56455950802590f3dff289abaffbbbac1633edb0/asreml_4.1.0.110.zip"},
                      win_3.6 = {"https://downloads.vsni.digital/4d3345fc4f8f72b379850da5ca56d39941bfec97/asreml_4.1.0.126.zip"},
                      win_4.0 = {"https://downloads.vsni.digital/a6e79b92e78e699132f22290fdd1bb3c2a32a2da/asreml_4.1.0.130.zip"},
                      win_4.1 = {"https://downloads.vsni.digital/a6e79b92e78e699132f22290fdd1bb3c2a32a2da/asreml_4.1.0.130.zip"},
                      mac_3.5 = {"https://downloads.vsni.digital/359627667d5176fdd2a4b2a559527edde2c8cb5f/asreml_4.1.0.110.tar.gz"},
                      mac_3.6 = {"https://downloads.vsni.digital/ddc4fb9f1358683716ab699940848b3079bb87ef/asreml_4.1.0.126.tar.gz"},
                      mac_4.0 = {"https://downloads.vsni.digital/298da6f73fb86bca5bb0aab73b48bd63a975bd85/asreml-4.1.0.130-macOS-10.13.2-R4.0.tar.gz"},
                      linux_3.5 = {"https://downloads.vsni.digital/ba605ae00be771c194a88d92f7bec9d6b4eeda72/asreml_Ubuntu-18_4.1.0.110.tar.gz"},
                      linux_3.6 = {"https://downloads.vsni.digital/cb8170325391a0d7908e6055e80827b4e7ed13d3/asreml_Ubuntu-18_4.1.0.126.tar.gz"},
                      linux_4.0 = {"https://downloads.vsni.digital/4749fd107b189d3d6ea52d39917eba660a3e542d/asreml-4.1.0.130-Ubuntu-18-R4.0.tar.gz"}
        )

        # Find position of the last / in the URL
        pos <- regexpr("\\/[^\\/]*$", url)

        # Extract everything after the last / as the filename
        filename <- substr(url, pos+1, nchar(url))
        save_path <- paste0(tempdir(), "\\", filename)

        # Download the file
        # pb <- progress::progress_bar$new(format = "Downloading ASreml-R: [:bar] :percent eta: :eta",
        #                                  total = 100, clear = FALSE, width= 60)
        # pb$tick(0)
        download.file(url, destfile = save_path, quiet = T)

        # Install asreml
        install.packages(save_path, lib = library, repos = NULL, quiet = T)

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
