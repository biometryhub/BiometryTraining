test_that("plotting works", {
    dat.aov <- aov(Petal.Length ~ Petal.Width, data = iris)

    expect_error(resplt(1:10), "mod.obj must be an aov or asreml object")

    if(R.version.string >= 3.3) {
        vdiffr::expect_doppelganger(title = "Plots produced", resplt(dat.aov))
        # vdiffr::manage_cases()
    }
})
