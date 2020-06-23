test_that("Installation works", {
    skip_if(R.version$status == "devel")
    expect_equal(install_asreml(), TRUE)
})

# test that
#   it installs
#   it returns TRUE when already installed
#   quiet works
