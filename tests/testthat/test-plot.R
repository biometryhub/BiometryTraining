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

  out3.1 <- plot.des(outdesign_crossed_lsd_plot,
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

  out4 <- plot.des(outdesign_crossed_rcbd_plot,
    nrows = 6,
    ncols = 6,
    brows = 1,
    bcols = 6,
    rotation = 0,
    size = 4,
    margin = F,
    return.seed = T,
    fac.sep = c("", " ")
  )

  out5 <- plot.des(outdesign_split_plot,
    nrows = 8,
    ncols = 4,
    brows = 8,
    bcols = 1,
    rotation = 0,
    size = 4,
    margin = F,
    return.seed = T,
  )

  # out6 <- plot.des(outdesign,
  #                  nrows = 11,
  #                  ncols = 4,
  #                  brows = 11,
  #                  bcols = 1,
  #                  rotation = 0,
  #                  size = 4,
  #                  margin = F,
  #                  return.seed = T)

  # Run vdiffr::manage_cases() on the console

  vdiffr::expect_doppelganger(title = "RCBD plot produced", out1$plot.des)
  vdiffr::expect_doppelganger(title = "LSD plot produced", out2$plot.des)
  vdiffr::expect_doppelganger(title = "Factorial LSD plot produced", out3$plot.des)
  vdiffr::expect_doppelganger(title = "Factorial RCBD plot produced", out4$plot.des)
  vdiffr::expect_doppelganger(title = "Split plot produced", out5$plot.des)
})
