test_that("designs are produced for supported types", {
    # CRD
    expect_output(design(type = "crd", treatments = c(1, 5, 10, 20),
                         reps = 5, nrows = 4, ncols = 5, seed = 42),
                  "Residual                                16\n")
    # RCBD
    expect_output(design("rcbd", treatments = LETTERS[1:11], reps = 4,
                         nrows = 11, ncols = 4, brows = 11, bcols = 1, seed = 42),
                  "Block stratum                           3\n")

    # LSD
    expect_output(design(type = "lsd", c("S1", "S2", "S3", "S4"),
                         nrows = 4, ncols = 4, seed = 42),
                  "Residual                                6\n")

    # Split
    expect_output(design(type = "split", treatments = c("A", "B"),
                         sub_treatments = 1:4, reps = 4, nrows = 8,
                         ncols = 4, brows = 4, bcols = 2, seed = 42,
                         fac.names = list(Water = c("Irrigated", "Rain-fed"),
                                          N = seq(50, 200, 50))),
                  "Whole plot Residual                          3\n")

    # Split with vector of names
    expect_output(design(type = "split", treatments = c("A", "B"),
                         sub_treatments = 1:4, reps = 4, nrows = 8,
                         ncols = 4, brows = 4, bcols = 2, seed = 42,
                         fac.names = c("Water", "Nitrogen")),
                  "Whole plot Residual                          3\n")

    # Crossed, CRD
    expect_output(design(type = "crossed:crd", treatments = c(3, 2),
                         reps = 3, nrows = 6, ncols = 3),
                  "A:B                                     2\n")

    # Crossed, CRD with renaming
    expect_output(design(type = "crossed:crd", treatments = c(3, 2),
                         reps = 3, nrows = 6, ncols = 3,
                         fac.names = list(N = c(50, 100, 150),
                                          Water = c("Irrigated", "Rain-fed"))),
                  "N:Water                                 2\n")

    # Crossed, RCBD
    expect_output(design(type = "crossed:rcbd", treatments = c(3, 2),
                         reps = 3, nrows = 6, ncols = 3, brows = 6, bcols = 1),
                  "Residual                                10\n")

    # Crossed, LSD with names
    expect_output(design(type = "crossed:lsd", treatments = c(3, 2),
                         nrows = 6, ncols = 6,
                         fac.names = list(N = c(50, 100, 150),
                                          Water = c("Irrigated", "Rain-fed"))),
                  "Row                                     5\n")
    # Nested, LSD
    expect_output(design(type = "lsd", treatments = c("A1", "A2", "A3", "A4", "B1", "B2", "B3"),
                         nrows = 7, ncols = 7, seed = 42),
                  "Residual                                30\n")
})

test_that("reps in lsd produces a message", {
    expect_message(design(type = "lsd", 1:4, reps = 3, nrows = 4, ncols = 4, seed = 42, quiet = TRUE),
                   "Number of replicates is not required for Latin Square designs")
})

test_that("unsupported design types give an error", {
    expect_error(
        design(type = "abc", 1:4, reps = 5, nrows = 4, ncols = 5, seed = 42),
        "Designs of type 'abc' are not supported"
    )
    expect_error(
        design(type = "strip", 1:4, reps = 5, nrows = 4, ncols = 5, seed = 42),
        "Designs of type 'strip' are not supported"
    )
    expect_error(
        design(type = "crossed:split", 1:4, reps = 5, nrows = 4, ncols = 5, seed = 42),
        "Crossed designs of type 'split' are not supported"
    )
    expect_error(
        design(type = "crossed:abc", 1:4, reps = 5, nrows = 4, ncols = 5, seed = 42),
        "Crossed designs of type 'abc' are not supported"
    )
    expect_error(
        design(type = "crossed:crd", treatments = 1:4, reps = 5, nrows = 4, ncols = 5, seed = 42),
        "Crossed designs with more than three treatment factors are not supported"
    )
})

test_that("split plot requires sub_treatments", {
    expect_error(design(type = "split", treatments = c("A", "B"),
                        sub_treatments = NULL, reps = 4, nrows = 8,
                        ncols = 4, brows = 4, bcols = 2, seed = 42),
                 "sub_treatments are required for a split plot design")
})

test_that("split plot requires brows and bcols", {
    expect_error(design(type = "split", treatments = c("A", "B"),
                        sub_treatments = 1:4, reps = 4, nrows = 8,
                        ncols = 4, brows = NA, bcols = 2, seed = 42),
                 "Design has blocks so brows and bcols must be supplied.")
})

test_that("passing unknown arguments to ggsave causes an error", {
    expect_error(
        design(type = "crd", treatments = c(1, 5, 10, 20),
               reps = 5, nrows = 4, ncols = 5, seed = 42, Width = 6),
        "1 components of `...` were not used."
    )
})



# type, treatments, reps, nrows, ncols, brows = NA, bcols = NA,
# rotation = 0, size = 4, margin = FALSE, save = FALSE, savename = paste0(type, "_design"),
# plottype = "pdf", seed = TRUE, quiet = FALSE, fac_names = NULL, ...


# test_that("missing bcols or brows gives an error", {
#     expect_error(
#         des.info(
#             design.obj = outdesign, nrows = 11,
#             ncols = 4, brows = NA, bcols = NA
#         ),
#         "Design has blocks so brows and bcols must be supplied."
#     )
#
#     expect_error(
#         des.info(
#             design.obj = outdesign_fac, nrows = 6,
#             ncols = 3, brows = NA, bcols = NA
#         ),
#         "Design has blocks so brows and bcols must be supplied."
#     )
# })
#
# test_that("unsupported design types give an error", {
#     expect_error(
#         des.info(
#             design.obj = design.strip(trt1 = trt, trt2 = letters[1:3], r = rep),
#             nrows = 12, ncols = 7
#         ),
#         "Designs of type 'strip' are not supported."
#     )
#
#     expect_output(
#         cat(des.info(design.ab(c(3, 2), r = 3, design = "crd"), nrows = 6, ncols = 3, quiet = T)$satab),
#         "Source of Variation"
#     )
# })
#
# test_that("save works with all the options", {
#     expect_error(
#         des.info(
#             design.obj = outdesign_crd,
#             nrows = 11,
#             ncols = 4,
#             save = "abc",
#             quiet = T
#         ),
#         "save must be one of 'none'/FALSE, 'both'/TRUE, 'plot', or 'workbook'."
#     )
#
#     # 'none' produces nothing
#     expect_file(des.info, list(
#         design.obj = outdesign_crd,
#         nrows = 11,
#         ncols = 4,
#         save = "none",
#         quiet = T
#     ),
#     "crd_design.csv",
#     missing = T
#     )
#     expect_file(des.info, list(
#         design.obj = outdesign_crd,
#         nrows = 11,
#         ncols = 4,
#         save = "none",
#         quiet = T
#     ),
#     "crd_design.pdf",
#     missing = T
#     )
#
#     # FALSE produces nothing
#     expect_file(des.info, list(
#         design.obj = outdesign_crd,
#         nrows = 11,
#         ncols = 4,
#         save = FALSE,
#         quiet = T
#     ),
#     "crd_design.csv",
#     missing = T
#     )
#     expect_file(des.info, list(
#         design.obj = outdesign_crd,
#         nrows = 11,
#         ncols = 4,
#         save = FALSE,
#         quiet = T
#     ),
#     "crd_design.pdf",
#     missing = T
#     )
#
#     if (file.exists("crd_design.csv")) file.remove("crd_design.csv")
#     if (file.exists("crd_design.pdf")) file.remove("crd_design.pdf")
#
#     # 'workbook' produces csv file and not plot
#     expect_file(
#         des.info, list(
#             design.obj = outdesign_crd,
#             nrows = 11,
#             ncols = 4,
#             save = "workbook",
#             quiet = T
#         ),
#         "crd_design.csv"
#     )
#     expect_file(des.info, list(
#         design.obj = outdesign_crd,
#         nrows = 11,
#         ncols = 4,
#         save = "workbook",
#         quiet = T
#     ),
#     "crd_design.pdf",
#     missing = T
#     )
#
#
#     if (file.exists("crd_design.csv")) file.remove("crd_design.csv")
#     if (file.exists("crd_design.pdf")) file.remove("crd_design.pdf")
#
#     # 'plot' produces plot file and not csv
#     expect_file(des.info, list(
#         design.obj = outdesign_crd,
#         nrows = 11,
#         ncols = 4,
#         save = "plot",
#         quiet = T
#     ),
#     "crd_design.csv",
#     missing = T
#     )
#     expect_file(
#         des.info, list(
#             design.obj = outdesign_crd,
#             nrows = 11,
#             ncols = 4,
#             save = "plot",
#             quiet = T
#         ),
#         "crd_design.pdf"
#     )
#
#     if (file.exists("crd_design.csv")) file.remove("crd_design.csv")
#     if (file.exists("crd_design.pdf")) file.remove("crd_design.pdf")
#
#     # 'plot' produces plot file and not csv
#     expect_file(
#         des.info, list(
#             design.obj = outdesign_crd,
#             nrows = 11,
#             ncols = 4,
#             save = "both",
#             quiet = T
#         ),
#         c("crd_design.csv", "crd_design.pdf")
#     )
#
#     if (file.exists("crd_design.csv")) file.remove("crd_design.csv")
#     if (file.exists("crd_design.pdf")) file.remove("crd_design.pdf")
#
#     # TRUE produces plot file and not csv
#     expect_file(
#         des.info, list(
#             design.obj = outdesign_crd,
#             nrows = 11,
#             ncols = 4,
#             save = TRUE,
#             quiet = T
#         ),
#         c("crd_design.csv", "crd_design.pdf")
#     )
# })
#
# test_that("savename works", {
#     expect_file(
#         des.info, list(
#             design.obj = outdesign_crd,
#             nrows = 11,
#             ncols = 4,
#             save = "both",
#             savename = "testfile",
#             quiet = T
#         ),
#         c("testfile.csv", "testfile.pdf")
#     )
# })
#
# test_that("plottype works", {
#     expect_file(
#         des.info, list(
#             design.obj = outdesign_crd,
#             nrows = 11,
#             ncols = 4,
#             save = "plot",
#             plottype = "png",
#             quiet = T
#         ),
#         "crd_design.png"
#     )
#
#     expect_file(
#         des.info, list(
#             design.obj = outdesign_crd,
#             nrows = 11,
#             ncols = 4,
#             save = "plot",
#             plottype = "jpeg",
#             quiet = T
#         ),
#         "crd_design.jpeg"
#     )
# })
#
# test_that("return.seed = T returns the seed of the design", {
#     expect_equal(des.info(
#         design.obj = outdesign_crd, nrows = 11,
#         ncols = 4, return.seed = T, quiet = T
#     )$seed, 42)
#
#     expect_null(des.info(
#         design.obj = outdesign_crd, nrows = 11,
#         ncols = 4, return.seed = F, quiet = T
#     )$seed)
#
#     expect_equal(
#         des.info(
#             design.obj = outdesign_crd_2, nrows = 11,
#             ncols = 4, return.seed = T, quiet = T
#         )$seed,
#         outdesign_crd_2$parameters$seed
#     )
# })
#
# test_that("passing arguments to ggsave works", {
#     expect_message(
#         des.info(design.obj = outdesign_crd, nrows = 11, ncols = 4, save = "plot", quiet = T, width = 8),
#         "Saving 8 x 7 in image"
#     )
# })
#
# test_that("quiet = F prints output and plot", {
#     expect_output(
#         des.info(design.obj = outdesign_crd, nrows = 11, ncols = 4),
#         "Source of Variation                     df"
#     )
#     x <- des.info(design.obj = outdesign_crd, nrows = 11, ncols = 4, quiet = T)
#
#     # skip_on_travis()
#     skip_if(getRversion() < 3.6)
#     vdiffr::expect_doppelganger("des_info output", x$plot.des)
# })
#


