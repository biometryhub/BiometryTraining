test_that("quiet supresses output", {
  expect_output(quiet(print("Hello")), NA)
})

test_that("Package message prints on load", {
    rlang::local_interactive(value = TRUE)
    expect_snapshot(BiometryTraining:::.onAttach(pkg = "BiometryTraining"))
})

test_that("Output prints if crayon is not installed", {
    rlang::local_interactive(value = TRUE)
    mockery::stub(BiometryTraining:::.onAttach, "rlang::is_installed", FALSE)
    expect_snapshot(BiometryTraining:::.onAttach(pkg = "BiometryTraining"))
})
