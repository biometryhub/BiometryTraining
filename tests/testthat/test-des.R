test_that("missing bcols or brows gives an error", {
    expect_error(des.info(design.obj = outdesign, nrows = 11,
                          ncols = 4, brows = NA, bcols = NA),
                 "Design has blocks so brows and bcols must be supplied.")

    expect_error(des.info(design.obj = outdesign_fac, nrows = 6,
                          ncols = 3, brows = NA, bcols = NA),
                 "Design has blocks so brows and bcols must be supplied.")
})

test_that("unsupported design types give an error", {
    expect_error(des.info(design.obj = design.strip(trt1 = trt, trt2 = letters[1:3], r = rep),
                          nrows = 12, ncols = 7),
                 "Designs of type 'strip' are not supported.")

    expect_output(cat(des.info(design.ab(c(3,2), r = 3, design = "crd"), nrows = 6, ncols = 3, quiet = T)$satab),
                  "Source of Variation")
})

test_that("save works with all the options", {
    expect_error(des.info(design.obj = outdesign_crd,
                          nrows = 11,
                          ncols = 4,
                          save = "abc",
                          quiet = T),
                 "save must be one of 'none'/FALSE, 'both'/TRUE, 'plot', or 'workbook'.")

    # 'none' produces nothing
    expect_file(des.info, list(design.obj = outdesign_crd,
                               nrows = 11,
                               ncols = 4,
                               save = "none",
                               quiet = T),
                "crd_design.csv", missing = T)
    expect_file(des.info, list(design.obj = outdesign_crd,
                               nrows = 11,
                               ncols = 4,
                               save = "none",
                               quiet = T),
                "crd_design.pdf", missing = T)

    # FALSE produces nothing
    expect_file(des.info, list(design.obj = outdesign_crd,
                               nrows = 11,
                               ncols = 4,
                               save = FALSE,
                               quiet = T),
                "crd_design.csv", missing = T)
    expect_file(des.info, list(design.obj = outdesign_crd,
                               nrows = 11,
                               ncols = 4,
                               save = FALSE,
                               quiet = T),
                "crd_design.pdf", missing = T)

    if (file.exists("crd_design.csv")) file.remove("crd_design.csv")
    if (file.exists("crd_design.pdf")) file.remove("crd_design.pdf")

    # 'workbook' produces csv file and not plot
    expect_file(des.info, list(design.obj = outdesign_crd,
                               nrows = 11,
                               ncols = 4,
                               save = "workbook",
                               quiet = T),
                "crd_design.csv")
    expect_file(des.info, list(design.obj = outdesign_crd,
                               nrows = 11,
                               ncols = 4,
                               save = "workbook",
                               quiet = T),
                "crd_design.pdf", missing = T)


    if (file.exists("crd_design.csv")) file.remove("crd_design.csv")
    if (file.exists("crd_design.pdf")) file.remove("crd_design.pdf")

    # 'plot' produces plot file and not csv
    expect_file(des.info, list(design.obj = outdesign_crd,
                               nrows = 11,
                               ncols = 4,
                               save = "plot",
                               quiet = T),
                "crd_design.csv", missing = T)
    expect_file(des.info, list(design.obj = outdesign_crd,
                               nrows = 11,
                               ncols = 4,
                               save = "plot",
                               quiet = T),
                "crd_design.pdf")

    if (file.exists("crd_design.csv")) file.remove("crd_design.csv")
    if (file.exists("crd_design.pdf")) file.remove("crd_design.pdf")

    # 'plot' produces plot file and not csv
    expect_file(des.info, list(design.obj = outdesign_crd,
                               nrows = 11,
                               ncols = 4,
                               save = "both",
                               quiet = T),
                c("crd_design.csv", "crd_design.pdf"))

    if (file.exists("crd_design.csv")) file.remove("crd_design.csv")
    if (file.exists("crd_design.pdf")) file.remove("crd_design.pdf")

    # TRUE produces plot file and not csv
    expect_file(des.info, list(design.obj = outdesign_crd,
                               nrows = 11,
                               ncols = 4,
                               save = TRUE,
                               quiet = T),
                c("crd_design.csv", "crd_design.pdf"))
})

test_that("savename works", {
    expect_file(des.info, list(design.obj = outdesign_crd,
                               nrows = 11,
                               ncols = 4,
                               save = "both",
                               savename = "testfile",
                               quiet = T),
                c("testfile.csv", "testfile.pdf"))
})

test_that("plottype works", {
    expect_file(des.info, list(design.obj = outdesign_crd,
                               nrows = 11,
                               ncols = 4,
                               save = "plot",
                               plottype = "png",
                               quiet = T),
                "crd_design.png")

    expect_file(des.info, list(design.obj = outdesign_crd,
                               nrows = 11,
                               ncols = 4,
                               save = "plot",
                               plottype = "jpeg",
                               quiet = T),
                "crd_design.jpeg")
})

test_that("return.seed = T returns the seed of the design", {
    expect_equal(des.info(design.obj = outdesign_crd, nrows = 11,
                          ncols = 4, return.seed = T, quiet = T)$seed, 42)

    expect_null(des.info(design.obj = outdesign_crd, nrows = 11,
                         ncols = 4, return.seed = F, quiet = T)$seed)

    expect_equal(des.info(design.obj = outdesign_crd_2, nrows = 11,
                          ncols = 4, return.seed = T, quiet = T)$seed,
                 outdesign_crd_2$parameters$seed)
})

test_that("passing arguments to ggsave works", {
    expect_message(des.info(design.obj = outdesign_crd, nrows = 11, ncols = 4, save = "plot", quiet = T, width = 8),
                   "Saving 8 x 7 in image")
})

test_that("quiet = F prints output and plot", {
    expect_output(des.info(design.obj = outdesign_crd, nrows = 11, ncols = 4),
                  "Source of Variation                     df")
    x <- des.info(design.obj = outdesign_crd, nrows = 11, ncols = 4, quiet = T)

    if(R.version.string >= 3.3) {
        vdiffr::expect_doppelganger("des_info output", x$plot.des)
    }
})
