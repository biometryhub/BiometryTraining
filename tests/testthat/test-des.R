test_that("missing bcols or brows gives an error", {
  expect_error(des.info(design.obj = outdesign, nrows = 11,
                        ncols = 4, brows = NA, bcols = NA),
               "Design has blocks so brows and bcols must be supplied.")
})

test_that("save works with all the options", {
    expect_error(des.info(design.obj = outdesign_crd, nrows = 11, ncols = 4, save = T),
                 "Design has blocks so brows and bcols must be supplied.")
})
