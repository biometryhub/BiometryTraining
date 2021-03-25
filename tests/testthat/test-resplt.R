# expect_plots_identical <- function(plot1, plot2) {
#   x <- as.raster(png::readPNG(plot1))
#   y <- as.raster(png::readPNG(plot2))
#   expect_true(identical(x, y))
# }



test_that("plotting works", {
  print(getwd())
  dat.aov <- aov(Petal.Length ~ Petal.Width, data = iris)

  expect_error(resplt(1:10), "mod.obj must be an aov or asreml object")
  # expect_error(resplt(dat.aov), "mod.obj must be an aov or asreml object")
  p1 <- resplot(dat.aov)


  load("../complexmodel.Rdata")
  p1_multi <- resplt(final.m.asr)[[1]]
  p2_multi <- resplot(final.m.asr)[[2]]
  p3_multi <- resplot(final.m.asr)[[3]]
  # ggsave("test-plot.png", p1)

  # expect_plots_identical("test-plot.png", "../figs/resplt/resplt-aov.png")

  skip_if(getRversion() < 3.6)
  vdiffr::expect_doppelganger(title = "Resplot for aov", p1)
  vdiffr::expect_doppelganger(title = "Resplot for asreml pt 1", p1_multi)
  vdiffr::expect_doppelganger(title = "Resplot for asreml pt 2", p2_multi)
  vdiffr::expect_doppelganger(title = "Resplot for asreml pt 3", p3_multi)
  # vdiffr::manage_cases()
})
