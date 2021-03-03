test_that("quiet supresses output", {
  expect_output(quiet(print("Hello")), NA)
})
