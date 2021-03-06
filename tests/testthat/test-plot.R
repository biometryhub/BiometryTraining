test_that("Plots output", {
  skip_if(getRversion() < 3.6)
  out1 <- plot.des(outdesign_rcbd_plot,
                   nrows = 11,
                   ncols = 4,
                   brows = 11,
                   bcols = 1,
                   rotation = 0,
                   size = 4,
                   margin = T,
                   return.seed = T
  )

  out1.1 <- plot.des(outdesign_rcbd_plot2,
                   nrows = 6,
                   ncols = 4,
                   brows = 3,
                   bcols = 4,
                   rotation = 0,
                   size = 4,
                   margin = F,
                   return.seed = T
  )

  out1.2 <- plot.des(outdesign_rcbd_plot3,
                   nrows = 6,
                   ncols = 4,
                   brows = 3,
                   bcols = 2,
                   rotation = 0,
                   size = 4,
                   margin = F,
                   return.seed = T
  )

  out1.3 <- plot.des(outdesign_rcbd_plot3,
                   nrows = 4,
                   ncols = 6,
                   brows = 1,
                   bcols = 6,
                   rotation = 0,
                   size = 4,
                   margin = F,
                   return.seed = T
  )

  out2 <- plot.des(outdesign_lsd_plot,
                   nrows = 4,
                   ncols = 4,
                   brows = NA,
                   bcols = NA,
                   rotation = 0,
                   size = 4,
                   margin = F,
                   return.seed = T
  )

  out3 <- plot.des(outdesign_crossed_lsd_plot,
                   nrows = 6,
                   ncols = 6,
                   brows = NA,
                   bcols = NA,
                   rotation = 0,
                   size = 4,
                   margin = F,
                   return.seed = T,
                   fac.sep = "_"
  )

  out4 <- plot.des(outdesign_crossed_plot,
                   nrows = 3,
                   ncols = 6,
                   rotation = 0,
                   size = 4,
                   margin = F,
                   return.seed = T,
                   fac.sep = c("", "")
  )

  out4.1 <- plot.des(outdesign_crossed_lsd_plot,
                     nrows = 6,
                     ncols = 6,
                     brows = NA,
                     bcols = NA,
                     rotation = 0,
                     size = 4,
                     margin = F,
                     return.seed = T,
                     fac.sep = c("", "_")
  )

  out4.2 <- plot.des(outdesign_crossed_lsd_plot,
                     nrows = 6,
                     ncols = 6,
                     brows = NA,
                     bcols = NA,
                     rotation = 0,
                     size = 4,
                     margin = F,
                     return.seed = T,
                     fac.sep = c("", "")
  )

  out5 <- plot.des(outdesign_crossed_rcbd_plot,
                   nrows = 6,
                   ncols = 3,
                   brows = 6,
                   bcols = 1,
                   rotation = 0,
                   size = 4,
                   margin = F,
                   return.seed = T,
                   fac.sep = c("", " ")
  )

  out5.1 <- plot.des(outdesign_crossed_rcbd_plot_2,
                     nrows = 3,
                     ncols = 6,
                     brows = 1,
                     bcols = 6,
                     rotation = 0,
                     size = 4,
                     margin = F,
                     return.seed = T,
                     fac.sep = c(":", "")
  )

  out5.2 <- plot.des(outdesign_crossed_rcbd_plot_2,
                     nrows = 6,
                     ncols = 3,
                     brows = 1,
                     bcols = 3,
                     rotation = 0,
                     size = 4,
                     margin = F,
                     return.seed = T,
                     fac.sep = c(":", "")
  )

  out5.3 <- plot.des(outdesign_crossed_rcbd_plot_3,
                     nrows = 6,
                     ncols = 4,
                     brows = 3,
                     bcols = 2,
                     rotation = 0,
                     size = 4,
                     margin = F,
                     return.seed = T,
                     fac.sep = c(":", "")
  )

  out6 <- plot.des(outdesign_split_plot,
                   nrows = 8,
                   ncols = 4,
                   brows = 8,
                   bcols = 1,
                   rotation = 0,
                   size = 4,
                   margin = F,
                   return.seed = T,
  )

  out6.1 <- plot.des(outdesign_split_plot,
                     nrows = 8,
                     ncols = 4,
                     brows = 1,
                     bcols = 4,
                     rotation = 0,
                     size = 4,
                     margin = F,
                     return.seed = T,
  )

  out6.2 <- plot.des(outdesign_split_plot,
                     nrows = 4,
                     ncols = 8,
                     brows = 1,
                     bcols = 8,
                     rotation = 0,
                     size = 4,
                     margin = F,
                     return.seed = T,
  )

  # Run vdiffr::manage_cases() on the console

  vdiffr::expect_doppelganger(title = "RCBD plot produced", out1$plot.des)
  vdiffr::expect_doppelganger(title = "RCBD plot row blocks", out1.1$plot.des)
  vdiffr::expect_doppelganger(title = "RCBD plot square blocks", out1.2$plot.des)
  vdiffr::expect_doppelganger(title = "RCBD plot long row blocks", out1.3$plot.des)
  vdiffr::expect_doppelganger(title = "LSD plot produced", out2$plot.des)
  vdiffr::expect_doppelganger(title = "Factorial LSD plot with sep", out3$plot.des)
  vdiffr::expect_doppelganger(title = "Factorial CRD plot no space sep", out4$plot.des)
  vdiffr::expect_doppelganger(title = "Factorial LSD plot underscore", out4.1$plot.des)
  vdiffr::expect_doppelganger(title = "Factorial LSD plot no space", out4.2$plot.des)
  vdiffr::expect_doppelganger(title = "Factorial RCBD plot produced", out5$plot.des)
  vdiffr::expect_doppelganger(title = "Factorial RCBD plot row blocks", out5.1$plot.des)
  vdiffr::expect_doppelganger(title = "Factorial RCBD plot double row blocks", out5.2$plot.des)
  vdiffr::expect_doppelganger(title = "Factorial RCBD plot square blocks", out5.2$plot.des)
  vdiffr::expect_doppelganger(title = "Split plot produced", out6$plot.des)
  vdiffr::expect_doppelganger(title = "Split plot double row blocks", out6.1$plot.des)
  vdiffr::expect_doppelganger(title = "Split plot ntrt == bcol", out6.2$plot.des)
})
