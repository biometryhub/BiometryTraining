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
                         nrows = 7, ncols = 7, seed = FALSE),
                  "Residual                                30\n")
})

test_that("reps in lsd produces a message", {
    expect_message(x <- design(type = "lsd", 1:4, reps = 3, nrows = 4, ncols = 4, seed = 42, quiet = TRUE),
                   "Number of replicates is not required for Latin Square designs and has been ignored")
})

test_that("rcbd requires brows and bcols", {
    expect_error(design("rcbd", treatments = LETTERS[1:11],
                        reps = 4, nrows = 11, ncols = 4,
                        brows = NA, bcols = 1, seed = 42),
                 "Design has blocks so brows and bcols must be supplied.")
    expect_error(design(type = "crossed:rcbd", treatments = c(3, 2),
                        reps = 3, nrows = 6, ncols = 3, brows = NA, bcols = 1),
                 "Design has blocks so brows and bcols must be supplied.")
})

test_that("unsupported design types give an error", {
    expect_error(design(type = "abc", 1:4, reps = 5,
                        nrows = 4, ncols = 5, seed = 42),
                 "Designs of type 'abc' are not supported")

    expect_error(design(type = "strip", 1:4, reps = 5,
                        nrows = 4, ncols = 5, seed = 42),
                 "Designs of type 'strip' are not supported")

    expect_error(design(type = "crossed:split", 1:4, reps = 5,
                        nrows = 4, ncols = 5, seed = 42),
                 "Crossed designs of type 'split' are not supported")

    expect_error(design(type = "crossed:abc", 1:4, reps = 5,
                        nrows = 4, ncols = 5, seed = 42),
                 "Crossed designs of type 'abc' are not supported")

    expect_error(design(type = "crossed:crd", treatments = 1:4,
                        reps = 5, nrows = 4, ncols = 5, seed = 42),
                 "Crossed designs with more than three treatment factors are not supported")
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

test_that("split plot produces warning when incorrect number of treatment labels given", {
    expect_warning(design(type = "split", treatments = c("A", "B"),
                          sub_treatments = 1:4, reps = 4, nrows = 8,
                          ncols = 4, brows = 4, bcols = 2, seed = 42,
                          fac.names = list(Water = "ABC",
                                           N = 1:4)),
                   "Water must contain the correct number of elements. Elements have not been applied.")
    expect_warning(design(type = "split", treatments = c("A", "B"),
                          sub_treatments = 1:4, reps = 4, nrows = 8,
                          ncols = 4, brows = 4, bcols = 2, seed = 42,
                          fac.names = list(Water = c("A", "B"),
                                           N = 1:10)),
                   "N must contain the correct number of elements. Elements have not been applied.")
    expect_warning(design(type = "split", treatments = c("A", "B"),
                          sub_treatments = 1:4, reps = 4, nrows = 8,
                          ncols = 4, brows = 4, bcols = 2, seed = 42,
                          fac.names = list(Water = c("A", "B"),
                                           N = 1:4,
                                           Another = 1:5)),
                   "fac.names contains 3 elements but only the first 2 have been used.")

    expect_warning(design(type = "split", treatments = c("A", "B"),
                          sub_treatments = 1:4, reps = 4, nrows = 8,
                          ncols = 4, brows = 4, bcols = 2, seed = 42,
                          fac.names = list(Water = c("A", "B"))),
                   "fac.names doesn't contain enough elements and has not been used.")
})



test_that("factorial designs produce warnings when incorrect number of treatment labels given", {
    expect_warning(design(type = "crossed:rcbd", treatments = c(3, 2),
                          reps = 3, nrows = 6, ncols = 3, brows = 6, bcols = 1,
                          fac.names = list(Water = c("A", "B"), N = 1:2), quiet = TRUE),
                   "Water must contain the correct number of elements. Elements have not been applied.")

    expect_warning(design(type = "crossed:rcbd", treatments = c(3, 2),
                          reps = 3, nrows = 6, ncols = 3, brows = 6, bcols = 1,
                          fac.names = list(Water = c("A", "B", "C"), N = 1), quiet = TRUE),
                   "N must contain the correct number of elements. Elements have not been applied.")

    expect_warning(design(type = "crossed:rcbd", treatments = c(3, 2),
                          reps = 3, nrows = 6, ncols = 3, brows = 6, bcols = 1,
                          fac.names = list(Water = c("A", "B", "C"), N = 1:2, Another = 1:10), quiet = TRUE),
                   "fac.names contains 3 elements but only the first 2 have been used.")

    expect_warning(design(type = "crossed:rcbd", treatments = c(3, 2),
                          reps = 3, nrows = 6, ncols = 3, brows = 6, bcols = 1,
                          fac.names = list(Water = c("A", "B", "C")), quiet = TRUE),
                   "fac.names doesn't contain enough elements and has not been used.")

    expect_warning(design(type = "crossed:crd", treatments = c(2, 2, 2),
                          reps = 3, nrows = 6, ncols = 4,
                          fac.names = list(Water = c("A", "B"), N = 1:2, Another = 1), quiet = T),
                   "Another must contain the correct number of elements. Elements have not been applied.")
})

test_that("passing unknown arguments to ggsave causes an error", {
    expect_error(
        design(type = "crd", treatments = c(1, 5, 10, 20),
               reps = 5, nrows = 4, ncols = 5, seed = 42, Width = 6),
        "1 components of `...` were not used."
    )
})

test_that("3 way factorial designs are possible", {
    expect_output(design(type = "crossed:crd", treatments = c(2, 2, 2),
                         reps = 3, nrows = 6, ncols = 4),
                  "A:B:C                                   1\n")
    expect_output(design(type = "crossed:crd", treatments = c(2, 2, 2),
                         reps = 3, nrows = 6, ncols = 4,
                         fac.names = list(X = c("A", "B"), Y = 1:2, Z = c(10, 20))),
                  "X:Y:Z                                   1\n")
})

test_that("Area and treatment size mismatches produce warnings", {
    # Wrap this in supressWarnings to hide other warning message
    suppressWarnings(expect_warning(
        design(type = "crd", treatments = c(1, 5, 10, 20),
               reps = 5, nrows = 4, ncols = 50, seed = 42, quiet = T),
        "Area provided is larger than treatments applied. Please check inputs."
    ))
    expect_warning(
        design(type = "crd", treatments = c(1, 5, 10, 20),
               reps = 5, nrows = 2, ncols = 5, seed = 42, quiet = T),
        "Area provided is smaller than treatments applied. Please check inputs."
    )
})

test_that("invalid save option produces an error", {
    expect_error(design("crd", treatments = 1:11, reps = 4, nrows = 11,
                        ncols = 4, save = "abc", quiet = TRUE),
                 "save must be one of 'none'/FALSE, 'both'/TRUE, 'plot', or 'workbook'."
    )
})

test_that("save = 'none' produces nothing", {
    design("crd", treatments = 1:11, reps = 4, nrows = 11, ncols = 4, save = "none", quiet = TRUE)
    expect_false(file.exists("crd_design.csv"))
    expect_false(file.exists("crd_design.pdf"))
})

test_that("save = FALSE produces nothing", {
    design("crd", treatments = 1:11, reps = 4, nrows = 11, ncols = 4, save = FALSE, quiet = TRUE)
    expect_false(file.exists("crd_design.csv"))
    expect_false(file.exists("crd_design.pdf"))
})

test_that("save = 'workbook' produces csv file and not plot", {
    design("crd", treatments = 1:11, reps = 4, nrows = 11, ncols = 4, save = "workbook", savename = "crd_design1", quiet = TRUE)
    withr::local_file("crd_design1.csv")
    expect_true(file.exists("crd_design1.csv"))
    expect_snapshot_file("crd_design1.csv")
    expect_false(file.exists("crd_design1.pdf"))
})

test_that("save = 'plot' produces plot file and not csv", {
    design("crd", treatments = 1:11, reps = 4, nrows = 11, ncols = 4, save = "plot", savename = "crd_design2", quiet = TRUE)
    withr::local_file("crd_design2.pdf")
    expect_false(file.exists("crd_design2.csv"))
    expect_true(file.exists("crd_design2.pdf"))
})

test_that("save = 'both' produces plot file and csv", {
    design("crd", treatments = 1:11, reps = 4, nrows = 11, ncols = 4, save = "both", savename = "crd_design3", quiet = TRUE)
    withr::local_file("crd_design3.pdf")
    withr::local_file("crd_design3.csv")
    expect_true(file.exists("crd_design3.csv"))
    expect_true(file.exists("crd_design3.pdf"))
    expect_snapshot_file("crd_design3.csv")
})

test_that("save = TRUE produces plot file and csv", {
    design("crd", treatments = 1:11, reps = 4, nrows = 11, ncols = 4, save = TRUE, savename = "crd_design4", quiet = TRUE)
    withr::local_file("crd_design4.pdf")
    withr::local_file("crd_design4.csv")
    expect_true(file.exists("crd_design4.csv"))
    expect_true(file.exists("crd_design4.pdf"))
    expect_snapshot_file("crd_design4.csv")
})
