skip_if(R.version$status == "Under development (unstable)")

test_that("Installation works", {
    skip_if(.Platform$OS.type == "windows")
    expect_message(install_asreml(force = T), "ASreml-R successfully installed!")
    expect_equal(install_asreml(), TRUE)
})

test_that("Update function works", {
    # skip_if(R.version$status == "Under development (unstable)")
    skip_if(.Platform$OS.type == "windows")
    expect_message(update_asreml(), "ASreml-R successfully installed!")
})

# test_that("Installation provides output on success", {
#     # skip_if(R.version$status == "Under development (unstable)")
#     skip_if(.Platform$OS.type == "windows")
#     if(requireNamespace("asreml", quietly = TRUE)){
#         unloadNamespace("asreml")
#         remove.packages("asreml")
#     }
#     expect_message(install_asreml(force = T), "ASreml-R successfully installed!")
# })

test_that("Returns true if asreml already installed", {
    # skip_if(R.version$status == "Under development (unstable)")
    skip_if(.Platform$OS.type == "windows")
    install_asreml(quiet=T)
    expect_equal(install_asreml(), TRUE)
})

test_that("Prints message if asreml already installed", {
    # skip_if(R.version$status == "Under development (unstable)")
    skip_if(.Platform$OS.type == "windows")
    install_asreml(quiet=T)
    expect_message(install_asreml(), "ASreml-R is already installed.")
})

test_that("Quiet returns no output", {
    skip_if(.Platform$OS.type == "windows")
    skip_if(R.version$status == "Under development (unstable)")
    expect_invisible(install_asreml(quiet = T))
    expect_invisible(install_asreml(quiet = T, force = T))
})

test_that("Force argument makes package install", {
    # skip_if(R.version$status == "Under development (unstable)")
    skip_if(.Platform$OS.type == "windows")
    install_asreml(force = T)
    expect_equal(requireNamespace("asreml"), TRUE)
    expect_message(install_asreml(force = T), NULL)
})

test_that("keep_file = F doesn't keep file", {
    # skip_if(R.version$status == "Under development (unstable)")
    skip_if(.Platform$OS.type == "windows")
    expect_file_2(install_asreml, list(force = T, keep_file = F),
                  pat = "asreml+(([a-zA-Z0-9_.\\-])*)+(.zip|.tar.gz|.tgz)", missing = T)
})

test_that("keep_file = T keeps file (in temp?)", {
    skip_if(.Platform$OS.type == "windows")
    # skip_if(R.version$status == "Under development (unstable)")
    expect_file_2(install_asreml, list(force = T, keep_file = T),
                  pat = "asreml+(([a-zA-Z0-9_.\\-])*)+(.zip|.tar.gz|.tgz)", dir = tempdir())
})


test_that("keep_file = 'data' keeps file in 'data'", {
    # skip_if(R.version$status == "Under development (unstable)")
    skip_if(.Platform$OS.type == "windows")
    dir.create(paste0(tempdir(), "/data"))
    expect_file_2(install_asreml, list(force = T, keep_file = paste0(tempdir(), "/data")),
                  pat = "asreml+(([a-zA-Z0-9_.\\-])*)+(.zip|.tar.gz|.tgz)", dir = paste0(tempdir(), "/data"))
})

test_that("Providing a non-existant directory fails", {
    # skip_if(R.version$status == "Under development (unstable)")
    skip_if(.Platform$OS.type == "windows")
    expect_error(install_asreml(force = T, keep_file = "abc"))
})

test_that("force and quiet work together", {
    # skip_if(R.version$status == "Under development (unstable)")
    skip_if(.Platform$OS.type == "windows")
    expect_invisible(install_asreml(force = T, quiet = T))
})


