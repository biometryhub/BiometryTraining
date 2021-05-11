test_that("Installation works", {
    skip_if(.Platform$OS.type == "windows")
    expect_message(install_asreml(force = TRUE), "ASReml-R successfully installed!")
    expect_equal(install_asreml(), TRUE)
})

test_that("Update function works", {
    # skip_if(R.version$status == "Under development (unstable)")
    skip_if(.Platform$OS.type == "windows")
    skip_on_cran()
    expect_message(update_asreml(), "ASReml-R successfully installed!")
})

# test_that("Installation provides output on success", {
#     # skip_if(R.version$status == "Under development (unstable)")
#     skip_if(.Platform$OS.type == "windows")
#     if(requireNamespace("asreml", quietly = TRUE)){
#         unloadNamespace("asreml")
#         remove.packages("asreml")
#     }
#     expect_message(install_asreml(force = T), "ASReml-R successfully installed!")
# })

test_that("Returns true if asreml already installed", {
    # skip_if(R.version$status == "Under development (unstable)")
    skip_if(.Platform$OS.type == "windows")
    install_asreml(quiet=TRUE)
    expect_equal(install_asreml(), TRUE)
})

test_that("Prints message if asreml already installed", {
    # skip_if(R.version$status == "Under development (unstable)")
    skip_if(.Platform$OS.type == "windows")
    install_asreml(quiet=TRUE)
    expect_message(install_asreml(), "ASReml-R is already installed.")
})

test_that("Quiet returns no output", {
    skip_if(.Platform$OS.type == "windows")
    skip_on_cran()
    expect_invisible(install_asreml(quiet = TRUE))
    expect_invisible(install_asreml(quiet = TRUE, force = TRUE))
})

test_that("Force argument makes package install", {
    # skip_if(R.version$status == "Under development (unstable)")
    skip_if(.Platform$OS.type == "windows")
    skip_on_cran()
    install_asreml(force = TRUE)
    expect_equal(requireNamespace("asreml"), TRUE)
    expect_message(install_asreml(force = TRUE), NULL)
})

test_that("keep_file = F doesn't keep file", {
    # skip_if(R.version$status == "Under development (unstable)")
    skip_if(.Platform$OS.type == "windows")
    skip_on_cran()
    expect_file_2(install_asreml, list(force = TRUE, keep_file = FALSE),
                  pattern = "asreml+(([a-zA-Z0-9_.\\-])*)+(.zip|.tar.gz|.tgz)", missing = T)
})

test_that("keep_file = T keeps file (in temp?)", {
    skip_if(.Platform$OS.type == "windows")
    skip_on_cran()
    # skip_if(R.version$status == "Under development (unstable)")
    expect_file_2(install_asreml, list(force = TRUE, keep_file = TRUE),
                  pattern = "asreml+(([a-zA-Z0-9_.\\-])*)+(.zip|.tar.gz|.tgz)")
})


test_that("keep_file = 'data' keeps file in 'data'", {
    # skip_if(R.version$status == "Under development (unstable)")
    skip_if(.Platform$OS.type == "windows")
    skip_on_cran()
    dir.create(paste0(tempdir(), "/data"))
    expect_file_2(install_asreml, list(force = TRUE, keep_file = paste0(tempdir(), "/data")),
                  pattern = "asreml+(([a-zA-Z0-9_.\\-])*)+(.zip|.tar.gz|.tgz)", dir = paste0(tempdir(), "/data"))
})

test_that("Providing a non-existant directory fails", {
    # skip_if(R.version$status == "Under development (unstable)")
    skip_if(.Platform$OS.type == "windows")
    skip_on_cran()
    expect_error(install_asreml(force = TRUE, keep_file = "abc"))
})

test_that("force and quiet work together", {
    # skip_if(R.version$status == "Under development (unstable)")
    skip_if(.Platform$OS.type == "windows")
    skip_on_cran()
    expect_invisible(install_asreml(force = TRUE, quiet = TRUE))
})


