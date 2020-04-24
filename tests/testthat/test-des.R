test_that("missing bcols or brows gives an error", {
  expect_error(des.info(design.obj = outdesign, nrows = 11,
                        ncols = 4, brows = NA, bcols = NA),
               "Design has blocks so brows and bcols must be supplied.")
})
