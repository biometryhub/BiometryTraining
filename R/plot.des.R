#' Produces graph of design layout
#'
#' @param design.obj An `agricolae` design object.
#' @param nrows The number of rows in the design.
#' @param ncols The number of columns in the design.
#' @param brows For RCBD only. The number of rows in a block.
#' @param bcols For RCBD only. The number of columns in a block.
#' @param byrow For split-plot only. Logical. Provides a way to arrange plots within whole-plots when there are multiple possible arrangements.
#' @param rotation Rotate the text output as Treatments within the plot. Allows for easier reading of long treatment labels. Number between 0 and 360 (inclusive) - default 0
#' @param size Increase or decrease the text size within the plot for treatment labels. Numeric with default value of 4.
#' @param margin Logical (default FALSE). Expand the plot to the edges of the plotting area i.e. remove whitespace between plot and axes.
#' @param quiet Logical (default FALSE). Return the objects without printing output.
#' @param return.seed Logical (default TRUE). Output the seed used in the design?
#' @param fac.sep Character to separate factorial treatments.
#'
#' @return Returns dataframe of design and ggplot object of design layout.
#'
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggplot2 ggplot geom_tile aes geom_text theme_bw scale_fill_manual scale_x_continuous scale_y_continuous scale_y_reverse
#' @importFrom scales reverse_trans
#' @importFrom stringi stri_sort
#' @keywords internal
#'
plot.des <- function(design.obj, ntrt, rotation, size, margin, return.seed, fac.sep) {
    # Asign NULL to variables that give a NOTE in package checks
    # Known issue. See https://www.r-bloggers.com/no-visible-binding-for-global-variable/
    treatments <- NULL
    xmin <- NULL
    xmax <- NULL
    ymin <- NULL
    ymax <- NULL
    Row <- NULL



    # des <- dplyr::mutate(des, row = factor(row),
    # row = factor(row, levels = rev(levels(row))))

    # create the colours for the graph
    color_palette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))(ntrt)

    if (!any(grepl("block", names(design.obj)))) {

        # create the graph
        plt <- ggplot2::ggplot() +
            ggplot2::geom_tile(data = design.obj, mapping = ggplot2::aes(x = col, y = row, fill = treatments), colour = "black") +
            ggplot2::geom_text(data = design.obj, mapping = ggplot2::aes(x = col, y = row, label = treatments), angle = rotation, size = size) +
            ggplot2::theme_bw() +
            ggplot2::scale_fill_manual(values = color_palette, name = "Treatment")
    }
    if (any(grepl("block", names(design.obj)))) {

        # Set up dataframe with coordinates for drawing the blocks
        blkdf <- data.frame(
            block = sort(unique(design.obj$block)),
            xmin = 0, xmax = 0, ymin = 0, ymax = 0
        )
        for (i in 1:nrow(blkdf)) {
            item <- blkdf$block[i]
            tmp <- design.obj[design.obj$block == item, ]
            blkdf[i, "ymin"] <- (min(tmp$row) - 0.5)
            blkdf[i, "ymax"] <- (max(tmp$row) + 0.5)
            blkdf[i, "xmin"] <- (min(tmp$col) - 0.5)
            blkdf[i, "xmax"] <- (max(tmp$col) + 0.5)
        }

        plt <- ggplot2::ggplot() +
            ggplot2::geom_tile(data = design.obj, mapping = ggplot2::aes(x = col, y = row, fill = treatments), colour = "black") +
            ggplot2::geom_text(data = design.obj, mapping = ggplot2::aes(x = col, y = row, label = treatments), angle = rotation, size = size) +
            #        xlim(0,max(des$col)+1) + ylim(0,max(des$row)+1) +
            ggplot2::geom_rect(
                data = blkdf,
                mapping = ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                size = 1.8, colour = "black", fill = NA
            ) +
            ggplot2::geom_rect(
                data = blkdf,
                mapping = ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                size = 0.6, colour = "white", fill = NA
            ) +
            ggplot2::theme_bw() +
            ggplot2::scale_fill_manual(values = color_palette, name = "Treatment") #+ ggplot2::scale_y_continuous(trans = scales::reverse_trans())
    }

    if (!margin) {
        plt <- plt + ggplot2::scale_x_continuous(expand = c(0, 0), breaks = seq(1, max(design.obj$col), 1)) + ggplot2::scale_y_continuous(expand = c(0, 0), trans = scales::reverse_trans(), breaks = seq(1, max(design.obj$row), 1))
    }
    else {
        plt <- plt + ggplot2::scale_y_continuous(trans = scales::reverse_trans(), breaks = seq(1, max(design.obj$row), 1)) + ggplot2::scale_x_continuous(breaks = seq(1, max(design.obj$col), 1))
    }

    return(plt)
}
