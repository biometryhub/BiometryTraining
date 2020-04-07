#' Produces graph of design layout, skeletal ANOVA table and data frame with complete design
#'
#' @param design.obj An \code{agricolae} design object.
#' @param nrows The number of rows in the design.
#' @param ncols The number of columns in the design.
#' @param brows For RCBD only. The number of rows in a block.
#' @param bcols For RCBD only. The number of columns in a block.
#' @param return.seed Logical (default TRUE). Output the seed used in the design?
#' @param rotation Rotate the text output as Treatments within the plot. Allows for easier reading of long treatment labels.
#' @param size Increase or decrease the text size within the plot for treatment labels. Numeric with default value of 4.
#' @param margin Logical (default FALSE). Expand the plot to the edges of the plotting area i.e. remove whitespace between plot and axes.
#' @param quiet Logical (default FALSE). Return the objects without printing output.
#'
#' @return A list containing a data frame with the complete design and a ggplot object with plot layout.
#'
#' @examples
#' library(agricolae)
#' trt <- c(1,5,10,20)
#' rep <- 5
#' outdesign <- design.crd(trt = trt, r=rep, seed = 42)
#' des.out <- des.info(design.obj = outdesign, nrows = 4,
#'                     ncols = 5, brows = NA, bcols = NA)
#'
#' trt <- LETTERS[1:11]
#' rep <- 4
#' outdesign <- design.rcbd(trt = trt, r = rep, seed = 42)
#' des.out <- des.info(design.obj = outdesign, nrows = 11,
#'                     ncols = 4, brows = 11, bcols = 1)
#'
#' @export
des.info <- function(design.obj, nrows, ncols, brows = NA, bcols = NA, return.seed = TRUE, rotation = 0, size = 4, margin = FALSE, quiet = FALSE){
    info <- plot.des(design.obj, nrows, ncols, brows, bcols, rotation, size, margin, quiet, return.seed = return.seed)
    info$satab <- satab(design.obj)

    if(!quiet) {
        satab(design.obj)
    }

    return(info)
}
