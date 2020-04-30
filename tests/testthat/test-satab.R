test_that("satab produces output", {
    expect_output(print(satab(outdesign_crd_satab)), "Source of Variation                     df")
    expect_output(print(satab(outdesign_rcbd_satab)), "Block stratum                           3")
    expect_output(print(satab(outdesign_lsd_satab)), "Row                                     3")
    expect_output(print(satab(outdesign_crossed_satab)), "AB                                      2")
    expect_output(print(satab(outdesign_nested_satab)), "Column                                  6")
    expect_output(print(satab(outdesign_split_satab)), "Whole plot Residual                          3")
})
