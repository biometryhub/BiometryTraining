#' @importFrom ggplot2 autoplot
#' @rdname autoplot
#' @export
ggplot2::autoplot

#' Automatic plots for objects generated in BiometryTraining
#'
#' @param object An object to create a plot for. Currently objects rom the [mct.out()] or [design()] functions with class "mct" or "design" respectively are supported.
#' @param label_height Height of the text labels above the upper error bar on the plot. Default is 0.1 (10%) of the difference between upper and lower error bars above the top error bar.
#' @param rotation Rotate the text output as Treatments within the plot. Allows for easier reading of long treatment labels. Number between 0 and 360 (inclusive) - default 0
#' @param size Increase or decrease the text size within the plot for treatment labels. Numeric with default value of 4.
#' @param margin Logical (default `FALSE`). A value of `FALSE` will expand the plot to the edges of the plotting area i.e. remove white space between plot and axes.
#' @param ... Other arguments to be passed through.
#'
#' @name autoplot
#'
#' @return A `ggplot2` object.
#' @seealso [mct.out()] and [design()]
#'
#' @examples
#' dat.aov <- aov(Petal.Width ~ Species, data = iris)
#' output <- mct.out(dat.aov, classify = "Species")
#' autoplot(output, label_height = 0.5)
NULL


#' @rdname autoplot
#' @importFrom ggplot2 autoplot ggplot aes_ aes geom_errorbar geom_text geom_point theme_bw labs theme element_text facet_wrap
#' @export
autoplot.mct <- function(object, rotation = 0, size = 4, label_height = 0.1, ...) {
    stopifnot(inherits(object, "mct"))
    if(!is.data.frame(object)) {
        object <- object$predicted_values
    }

    # classify is just the first n columns (before predicted.value)
    classify <- colnames(object)[1]
    if(colnames(object)[2] != "predicted.value") {
        classify2 <- colnames(object)[2]
    }
    if(colnames(object)[2] != "predicted.value" & colnames(object)[3] != "predicted.value") {
        classify3 <- colnames(object)[3]
    }

    # Get ylab as attribute
    ylab <- attributes(object)$ylab

    yval <- ifelse("PredictedValue" %in% colnames(object), "PredictedValue", "predicted.value")

    plot <- ggplot2::ggplot(data = object, ggplot2::aes_(x = as.name(classify))) +
        ggplot2::geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
        ggplot2::geom_text(ggplot2::aes_(x = as.name(classify), y = object$up, label = object$groups), vjust = 0, nudge_y = (object$up-object$low)*label_height, size = size) +
        ggplot2::geom_point(ggplot2::aes_(y = as.name(yval)), color = "black", shape = 16) + ggplot2::theme_bw() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = rotation)) +
        ggplot2::labs(x = "", y = paste0("Predicted ", ylab))

    if(exists("classify3")) {
        plot <- plot + ggplot2::facet_wrap(as.formula(paste("~", classify2, "+", classify3)))
    }
    else if(exists("classify2")) {
        plot <- plot + ggplot2::facet_wrap(as.formula(paste("~", classify2)))
    }
    return(plot)
}

#' @rdname autoplot
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggplot2 ggplot geom_tile aes geom_text theme_bw scale_fill_manual scale_x_continuous scale_y_continuous scale_y_reverse
#' @importFrom scales reverse_trans
#' @importFrom stringi stri_sort
#' @export
autoplot.design <- function(object, rotation = 0, size = 4, margin = FALSE, ...) {
    # Asign NULL to variables that give a NOTE in package checks
    # Known issue. See https://www.r-bloggers.com/no-visible-binding-for-global-variable/
    xmin <- NULL
    xmax <- NULL
    ymin <- NULL
    ymax <- NULL
    Row <- NULL

    # if(object)

    ntrt <- nlevels(as.factor(object$treatments))

    # create the colours for the graph
    color_palette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))(ntrt)

    if (!any(grepl("block", names(object)))) {

        # create the graph
        plt <- ggplot2::ggplot() +
            ggplot2::geom_tile(data = object, mapping = ggplot2::aes(x = col, y = row, fill = treatments), colour = "black") +
            ggplot2::geom_text(data = object, mapping = ggplot2::aes(x = col, y = row, label = treatments), angle = rotation, size = size) +
            ggplot2::theme_bw() +
            ggplot2::scale_fill_manual(values = color_palette, name = "Treatment")
    }
    if (any(grepl("block", names(object)))) {

        # Set up dataframe with coordinates for drawing the blocks
        blkdf <- data.frame(
            block = sort(unique(object$block)),
            xmin = 0, xmax = 0, ymin = 0, ymax = 0
        )
        for (i in 1:nrow(blkdf)) {
            item <- blkdf$block[i]
            tmp <- object[object$block == item, ]
            blkdf[i, "ymin"] <- (min(tmp$row) - 0.5)
            blkdf[i, "ymax"] <- (max(tmp$row) + 0.5)
            blkdf[i, "xmin"] <- (min(tmp$col) - 0.5)
            blkdf[i, "xmax"] <- (max(tmp$col) + 0.5)
        }

        plt <- ggplot2::ggplot() +
            ggplot2::geom_tile(data = object, mapping = ggplot2::aes(x = col, y = row, fill = treatments), colour = "black") +
            ggplot2::geom_text(data = object, mapping = ggplot2::aes(x = col, y = row, label = treatments), angle = rotation, size = size) +
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
            ggplot2::scale_fill_manual(values = color_palette, name = "Treatment")
    }

    if (!margin) {
        plt <- plt + ggplot2::scale_x_continuous(expand = c(0, 0), breaks = seq(1, max(object$col), 1)) + ggplot2::scale_y_continuous(expand = c(0, 0), trans = scales::reverse_trans(), breaks = seq(1, max(object$row), 1))
    }
    else {
        plt <- plt + ggplot2::scale_y_continuous(trans = scales::reverse_trans(), breaks = seq(1, max(object$row), 1)) + ggplot2::scale_x_continuous(breaks = seq(1, max(object$col), 1))
    }

    return(plt)
}

