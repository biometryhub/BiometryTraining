#' Produces graph of design layout, skeletal ANOVA table and data frame with complete design
#'
#' @param design.obj An `agricolae` design object.
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
#' @param return.seed Logical (default TRUE). Output the seed used in the design?
#' @param quiet Logical (default FALSE). Return the objects without printing output.
#' @param ... Additional parameters passed to [ggplot2::ggsave()] for saving the plot.
#'
#' @details If `save = TRUE` (or `"both"`), both the plot and the workbook will be saved to the current working directory, with filename given by `savename`. If one of either `"plot"` or `"workbook"` is specified, only that output is saved. If `save = FALSE` (the default, or equivalently `"none"`), nothing will be output.
#' @details `...` allows extra arguments to be passed to ggsave for output of the plot. The details of possible arguments can be found in  [ggplot2::ggsave()].
#'
#' @importFrom graphics plot
#' @importFrom ggplot2 ggsave
#' @importFrom utils write.csv
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
#' @export
#'
des.info <- function(design.obj, nrows, ncols, brows = NA, bcols = NA, rotation = 0, size = 4, margin = FALSE, save = FALSE, savename = paste0(design.obj$parameters$design, "_design"), plottype = "pdf", return.seed = TRUE, quiet = FALSE, ...) {

  # Error checking of inputs

  # Check design type is supported
  if (!design.obj$parameters$design %in% c("crd", "rcbd", "lsd", "factorial", "split")) {
    stop(paste0("Designs of type '", design.obj$parameters$design, "' are not supported."))
  }

  # Check brows and bcols supplied if necessary
  if (design.obj$parameters$design == "rcbd" & anyNA(c(brows, bcols))) {
    stop("Design has blocks so brows and bcols must be supplied.")
  }
  else if (design.obj$parameters$design == "factorial") {
    if (design.obj$parameters$applied == "rcbd" &
      anyNA(c(brows, bcols))) {
      stop("Design has blocks so brows and bcols must be supplied.")
    }
  }

  info <- plot.des(design.obj, nrows, ncols, brows, bcols, rotation, size, margin, return.seed = return.seed)
  info$satab <- satab(design.obj)

  if (!quiet) {
    cat(info$satab)
    plot(info$plot.des)
  }

  if (!is.logical(save)) {
    output <- tolower(save)
    if (output == "plot") {
      ggplot2::ggsave(filename = paste0(savename, ".", plottype), ...)
    }
    else if (output == "workbook") {
      write.csv(info$design, file = paste0(savename, ".csv"), row.names = F)
    }
    else if (output == "both") {
      ggplot2::ggsave(filename = paste0(savename, ".", plottype), ...)
      write.csv(info$design, file = paste0(savename, ".csv"), row.names = F)
    }
    else if (output == "none") {
      # Do nothing
    }
    else {
      stop("save must be one of 'none'/FALSE, 'both'/TRUE, 'plot', or 'workbook'.")
    }
  }
  else if (save) {
    ggplot2::ggsave(filename = paste0(savename, ".", plottype), ...)
    write.csv(info$design, file = paste0(savename, ".csv"), row.names = F)
  }

  return(info)
}
