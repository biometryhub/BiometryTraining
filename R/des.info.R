#' Produces graph of design layout, skeletal ANOVA table and data frame with complete design
#'
#' @param design.obj An \code{agricolae} design object.
#' @param nrows The number of rows in the design.
#' @param ncols The number of columns in the design.
#' @param brows For RCBD only. The number of rows in a block.
#' @param bcols For RCBD only. The number of columns in a block.
#'
#' @return A data frame with the complete design.
#'
#' @examples
#' library(agricolae)
#' trt <- c(1,5,10,20)
#' rep <- 5
#' outdesign <- design.crd(trt = trt, r=rep)
#' des.out <- des.info(design.obj = outdesign, nrows = 4,
#'                     ncols = 5, brows = NA, bcols = NA)
#'
#' trt <- LETTERS[1:11]
#' rep <- 4
#' outdesign <- design.rcbd(trt = trt, r = rep)
#' des.out <- des.info(design.obj = outdesign, nrows = 11,
#'                     ncols = 4, brows = 11, bcols = 1)
#'
des.info <- function(design.obj, nrows, ncols, brows = NA, bcols = NA){
    plt <- plot.des(design.obj, nrows, ncols, brows, bcols)
    satab(design.obj)

    return(plt)
}
