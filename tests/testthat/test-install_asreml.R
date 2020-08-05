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

test_that("force argument makes package install", {
  skip_if(R.version$status == "devel")
  expect_message(install_asreml(force = T, keep_file = T), "ASreml-R successfully installed!")
  expect_file(install_asreml(force = T, keep_file = T))
})

# install_asreml(force = T, keep_file = T) # Doesn't exist
# install_asreml(force = T, keep_file = T) # Exists in tempdir
# install_asreml(force = T, keep_file = T) # Exists in current dir, not in temp
# install_asreml(force = T, keep_file = "path") # Exists in temp dir, copy to path
# install_asreml(force = T, keep_file = "path") # Exists in current dir, copy to path


# test that
#   force and quiet combinations work
#   Installing to a different location works
#   Keep file works - current folder and alternative
#   Keep file works if file already exists in temp and current
#   Providing a non-existant directory fails
#   Update function works
