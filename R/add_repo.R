#' Add the drat repository for the Biometry Hub packages to .Rprofile to enable easy update and installation.
#'
#' @param CRAN Default CRAN repository to add. Full list of CRAN repositories can be found at [https://cran.r-project.org/mirrors.html](https://cran.r-project.org/mirrors.html).
#' @param ... Other repositories to add.
#'
#' @importFrom drat addRepo
#'
#' @keywords internal
add_repo <- function(CRAN = "https://cloud.r-project.org/", ...) {

    repos = c(CRAN = CRAN, unlist(list(...)))

    # Check if .Rprofile exists
    candidates <- c(Sys.getenv("R_PROFILE"),
                    Sys.getenv("R_PROFILE_USER"),
                    file.path(Sys.getenv("HOME"), ".Rprofile"),
                    file.path(getwd(), ".Rprofile"),
                    file.path(Sys.getenv("R_HOME"), "etc", "Rprofile.site"))

    profiles <- Filter(file.exists, candidates)

    # If not, create
    if(length(profiles)==0) {
        file.create("~/.Rprofile")
        writeLines()
    }
    # If so, update
    else {
        grep("repos", readLines(profiles[1])) #Do I need to do this for all .Rp files?
    }


    options(repos=structure(c(CRAN=CRAN)))
    drat::addRepo("biometryhub")

    r = getOption("repos")
    for (n in names(repos)) {
        r[n] = repos[n]
    }
    options(repos = r)
}
