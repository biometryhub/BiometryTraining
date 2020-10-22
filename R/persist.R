#' Persist the Biometry Hub drat repo across sessions
#'
#' Add the drat repository for the Biometry Hub packages to .Rprofile to enable easy update and installation even after restating R.
#'
#' @param CRAN Default CRAN repository to add. A complete list of CRAN repositories can be found at [https://cran.r-project.org/mirrors.html](https://cran.r-project.org/mirrors.html).
#' @param ... Other repositories to add. These can be listed using `name = URL` syntax, including drat repositories.
#' @param profile_location A path to a .Rprofile file specified by the user.
#'
#' @details The standard locations of .Rprofile files are searched (see [base::Startup()]) (mostly).
#'
#' @importFrom drat addRepo
#'
#' @export
persist <- function(..., cran = "https://cloud.r-project.org/", profile_location = NULL) {

    repos = c(CRAN = cran, unlist(list(...)))

    # Check if any repos are already set.
    # Should always return at least one set
    r = getOption("repos")

    # Flag for checking if CRAN is set by Rstudio
    cran_set <- ifelse("RStudio" %in% names(attributes(r)) && !is.null(r) && !is.na(r), TRUE, FALSE)

    # Check if .Rprofile exists
    candidates <- c(profile_location,
                    Sys.getenv("R_PROFILE_USER"),
                    Sys.getenv("R_PROFILE"),
                    file.path(Sys.getenv("HOME"), ".Rprofile"),
                    file.path(getwd(), ".Rprofile"))

    rps <- Filter(file.exists, candidates)

    # If not, create
    if(length(rps) == 0) {
        file.create("~/.Rprofile")
        rps[1] <- "~/.Rprofile"
    }

    # Then write lines to the first entry in rps
    if(cran_set) {
        # Rstudio has set CRAN, overwrite
        r["CRAN"] <- repos["CRAN"]
        attributes(r)$RStudio <- NULL

    }

    # Check if drat biometry hub repo already added
    if(!any(grepl('^(?!#)+\\s*drat::addRepo\\(\"biometryhub\"\\)', readLines(rps[1]), perl = T))) {
        r["biometryhub"] <- "https://biometryhub.github.io/drat/"
    }

    # Paste any other repos to the string
    r <- c(r, repos[-1])
    repos <- paste(names(r), r, sep = " = ", collapse = "', '")
    cat("options(repos = c('", repos, "'))\n", sep = "", file = rps[1], append = T)

    # drat::addRepo('biometryhub')
    invisible(r)
}
