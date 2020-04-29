test_that("plotting works", {
    vdiffr::expect_doppelganger(title = "Plots produced", resplt(dat.aov))
    # vdiffr::manage_cases()
})
