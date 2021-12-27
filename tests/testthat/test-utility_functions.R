test_that("quiet supresses output", {
  expect_output(quiet(print("Hello")), NA)
})


test_that("Package message prints on load", {
    rlang::local_interactive(value = TRUE)
    expect_snapshot(BiometryTraining:::.onAttach(pkg = "BiometryTraining"))
})

# test_that("Output prints if crayon is not installed", {
#     rlang::local_interactive(value = TRUE)
#     is_otherpackage_installed <- function(){
#         result <- requireNamespace("crayon", quietly = TRUE)
#         return(result)
#     }
#     mockery::stub(BiometryTraining:::.onAttach(pkg = "BiometryTraining"), "requireNamespace", FALSE)
#
#     expect_snapshot(BiometryTraining:::.onAttach(pkg = "BiometryTraining"))
# })
