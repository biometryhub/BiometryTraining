# expect_plots_identical <- function(plot1, plot2) {
#   x <- as.raster(png::readPNG(plot1))
#   y <- as.raster(png::readPNG(plot2))
#   expect_true(identical(x, y))
# }



test_that("plotting works", {
  dat.aov <- aov(Petal.Length ~ Petal.Width, data = iris)

  expect_error(resplt(1:10), "mod.obj must be an aov or asreml object")
  p1 <- resplot(dat.aov)

  skip_if(getRversion() < 3.6)
  vdiffr::expect_doppelganger(title = "Resplot for aov", p1)

  # skip_if_not_installed("asreml")
  # load("../complexmodel.Rdata")
  # p1_multi <- resplt(final.m.asr)#[[1]]
  # p2_multi <- resplot(final.m.asr)[[2]]
  # p3_multi <- resplot(final.m.asr)[[3]]
  # ggsave("test-plot.png", p1)

  # expect_plots_identical("test-plot.png", "../figs/resplt/resplt-aov.png")
  # vdiffr::expect_doppelganger(title = "Resplot for asreml pt 1", p1_multi[[1]])
  # vdiffr::expect_doppelganger(title = "Resplot for asreml pt 2", p1_multi[[2]])
  # vdiffr::expect_doppelganger(title = "Resplot for asreml pt 3", p1_multi[[3]])
  # vdiffr::manage_cases()
})
