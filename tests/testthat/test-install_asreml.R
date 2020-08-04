test_that("Installation works", {
  skip_if(R.version$status == "devel")
  expect_equal(install_asreml(), TRUE)
})
#
# test_that("Installation works", {
#   skip_if(R.version$status == "devel")
#   if("asreml" %in% installed.packages()[,1]){remove.packages("asreml")}
#   expect_message(install_asreml(), "ASreml-R successfully installed!")
# })

test_that("Returns true if asreml already installed", {
    skip_if(R.version$status == "devel")
    install_asreml(quiet=T)
    expect_equal(install_asreml(), TRUE)
    expect_message(install_asreml(), "ASreml-R is already installed.")
})

test_that("Quiet returns no output", {
  skip_if(R.version$status == "devel")
  expect_invisible(install_asreml(quiet = T))
})

test_that("force argument makes package install", {
  skip_if(R.version$status == "devel")
  expect_equal(install_asreml(force = T), TRUE)
  expect_message(install_asreml(force = T), "ASreml-R successfully installed!")
})

# test that
#   force and quiet combinations work
#   Installing to a different location works
#   Keep file works - current folder and alternative
#   Providing a non-existant directory fails
#   Update function works
