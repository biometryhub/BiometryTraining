#' Produces an experimental design with graph of design layout and skeletal ANOVA table
#'
#' @param type The type of design. Supported design types are `crd`, `rcbd`, `lsd`, `split` and `crossed:<type>` where `<type>` is one of the previous types. See Details for more informtaion.
#' @param trt A vector containing the treatment names or labels.
#' @param reps The number of replicates. Not required for Latin Squared Designs.
#' @param nrows The number of rows in the design.
#' @param ncols The number of columns in the design.
#' @param brows For RCBD only. The number of rows in a block.
#' @param bcols For RCBD only. The number of columns in a block.
#' @param rotation Rotate the text output as Treatments within the plot. Allows for easier reading of long treatment labels. Takes positive and negative values being number of degrees of rotation from horizontal.
#' @param size Increase or decrease the text size within the plot for treatment labels. Numeric with default value of 4.
#' @param margin Logical (default FALSE). Expand the plot to the edges of the plotting area i.e. remove white space between plot and axes.
#' @param save One of `FALSE` (default)/`"none"`, `TRUE`/`"both"`, `"plot"` or `"workbook"`. Specifies which output to save.
#' @param savename A filename for the design to be saved to. Default is the type of the design combined with "_design".
#' @param plottype The type of file to save the plot as. Usually one of `"pdf"`, `"png"`, or `"jpg"`. See [ggplot2::ggsave()] for all possible options.
#' @param seed If `TRUE` (the default), return the seed used to generate the design. If a numeric value, use that value as the seed for the design.
#' @param quiet Logical (default FALSE). Return the objects without printing output.
#' @param fac_names Allows renaming of the `A` level of factorial designs (i.e. those using [agricolae::design.ab()]) by passing (optionally named) vectors of new labels to be applied to the factors within a list. See examples and details for more information.
#' @param ... Additional parameters passed to [ggplot2::ggsave()] for saving the plot.
#'
#' @details The designs currently supported by `type` are Completely Randomised designs (`crd`), Randomised Complete Block designs (`rcbd`), Latin Square Designs (`lsd`), Split Plot designs (`split`) and Factorial with crossed structure (`crossed:<type>` where `<type>` is one of the previous types (e.g. `crossed:crd`). Nested factorial designs are supported through manual setup, see Examples.
#' @details If `save = TRUE` (or `"both"`), both the plot and the workbook will be saved to the current working directory, with filename given by `savename`. If one of either `"plot"` or `"workbook"` is specified, only that output is saved. If `save = FALSE` (the default, or equivalently `"none"`), nothing will be output.
#' @details `fac_names` can be supplied to provide more intuitive names for factors and their levels in factorial designs. They should be specified in a list format, for example `fac_names = list(A_names = c("a", "b", "c"), B_names = c("x", "y", "z"))`. This will result a design output with a column named `A_names` with levels `a, b, c` and another named `B_names` with levels `x, y, z`. Only the first two elements of the list will be used.
#' @details `...` allows extra arguments to be passed to ggsave for output of the plot. The details of possible arguments can be found in  [ggplot2::ggsave()].
#'
#' @importFrom graphics plot
#' @importFrom ggplot2 ggsave
#' @importFrom utils write.csv
#'
#' @export
#'
#' @return A list containing a data frame with the complete design, a ggplot object with plot layout, the seed (if `return.seed = TRUE`), and the `satab` object, allowing repeat output of the `satab` table via `cat(output$satab)`.
#'
#' @examples
#' library(agricolae)
#'
#' # Completely Randomised Design
#' trt <- c(1, 5, 10, 20)
#' rep <- 5
#' outdesign <- design.crd(trt = trt, r = rep, seed = 42)
#' des.out <- des.info(design.obj = outdesign, nrows = 4, ncols = 5)
#'
#' # Randomised Complete Block Design
#' trt <- LETTERS[1:11]
#' rep <- 4
#' outdesign <- design.rcbd(trt = trt, r = rep, seed = 42)
#' des.out <- des.info(
#'   design.obj = outdesign, nrows = 11,
#'   ncols = 4, brows = 11, bcols = 1
#' )
#'
#' # Latin Square Design
#' trt <- c("S1", "S2", "S3", "S4")
#' outdesign <- design.lsd(trt)
#' des.out <- des.info(design.obj = outdesign, nrows = 4, ncols = 4)
#'
#' # Factorial Design (Crossed, Completely Randomised)
#' trt <- c(3, 2) # Factorial 3 x 2
#' rep <- 3
#' outdesign <- design.ab(trt, r = rep, design = "crd")
#' des.out <- des.info(design.obj = outdesign, nrows = 6, ncols = 3)
#'
#' # Factorial Design (Crossed, Completely Randomised), renaming factors
#' trt <- c(3, 2) # Factorial 3 x 2
#' rep <- 3
#' outdesign <- design.ab(trt, r = rep, design = "crd")
#' des.out <- des.info(design.obj = outdesign, nrows = 6, ncols = 3,
#' fac_names = list(N = c(50, 100, 150), Water = c("Irrigated", "Rain-fed")))
#'
#' # Factorial Design (Nested, Latin Square)
#' trt <- c("A1", "A2", "A3", "A4", "B1", "B2", "B3")
#' outdesign <- design.lsd(trt)
#' des.out <- des.info(design.obj = outdesign, nrows = 7, ncols = 7)
#'
#' # Split plot design
#' trt1 <- c("A", "B")
#' trt2 <- 1:4
#' rep <- 4
#' outdesign <- design.split(trt1, trt2, r = rep)
#' des.out <- des.info(design.obj = outdesign, nrows = 8, ncols = 4, brows = 4, bcols = 2)
#'
design <- function(type,
                   treatments,
                   treatments2,
                   reps,
                   nrows,
                   ncols,
                   brows = NA,
                   bcols = NA,
                   rotation = 0,
                   size = 4,
                   margin = FALSE,
                   save = FALSE,
                   savename = paste0(design.obj$parameters$design, "_design"),
                   plottype = "pdf",
                   seed = TRUE,
                   quiet = FALSE,
                   fac_names = NULL,
                   ...) {

    # Generate design based on type input
    # If seed is numeric, use that seed to generate the design. If seed is TRUE,

    if(tolower(type) == "crd") {
        outdesign <- agricolae::design.crd(trt = treatments,
                                           r = reps,
                                           seed = ifelse(is.numeric(seed), seed, NULL))
    }

    else if(tolower(type) == "rcbd") {
        outdesign <- agricolae::design.rcbd(trt = treatments,
                                            r = reps,
                                            seed = ifelse(is.numeric(seed), seed, NULL))
    }

    else if(tolower(type) == "lsd") {
        outdesign <- agricolae::design.lsd(trt = treatments,
                                           seed = ifelse(is.numeric(seed), seed, NULL))
        message("Number of replicates is not required for Latin Square designs.")
    }

    else if(tolower(type) == "split") {
        outdesign <- agricolae::design.crd(trt = treatments,
                                           r = reps,
                                           seed = ifelse(is.numeric(seed), seed, NULL))
    }

    else if(unlist(strsplit(tolower(type), ":"))[1] == "crossed") {
        type_split <- unlist(strsplit(tolower(type), ":"))

        if(type_split[2] %!in% c("crd", "rcbd", "lsd")) {
            stop(paste("Crossed designs of type", type_split[2], "are not supported."))
        }

        outdesign <- agricolae::design.ab(trt = treatments,
                                          r = reps,
                                          design = type_split[2],
                                          seed = ifelse(is.numeric(seed), seed, NULL))
    }

}
