test_that("plotting works", {
  dat.aov <- aov(Petal.Length ~ Petal.Width, data = iris)

  expect_error(resplt(1:10), "mod.obj must be an aov or asreml object")
  # expect_error(resplt(dat.aov), "mod.obj must be an aov or asreml object")

  skip_if(getRversion() < 3.6)
  # vdiffr::expect_doppelganger(title = "Plots produced", print(resplt(dat.aov)))
  # vdiffr::manage_cases()
})
