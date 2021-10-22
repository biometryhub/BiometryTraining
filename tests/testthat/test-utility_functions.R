test_that("quiet supresses output", {
  expect_output(quiet(print("Hello")), NA)
})


test_that("Package message prints on load", {
    expect_snapshot(BiometryTraining:::.onAttach(pkg = "BiometryTraining"))
})
