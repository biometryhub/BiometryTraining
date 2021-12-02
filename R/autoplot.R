#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot

#' Automatic plots for objects generated in BiometryTraining
#'
#' @param mct_object A data frame of output from the [mct.out()] function with class "mct".
#' @param design_object Output from the [design()] function with class "design".
#' @param label_height Height of the text labels above the upper error bar on the plot. Default is 0.1 (10%) of the difference between upper and lower error bars above the top error bar.
#' @param rotation Rotate the text output as Treatments within the plot. Allows for easier reading of long treatment labels. Number between 0 and 360 (inclusive) - default 0
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
autoplot.mct <- function(mct_object, label_height = 0.1, rotation = 0, ...) {
    stopifnot(inherits(mct_object, "mct"))
    if(!is.data.frame(mct_object)) {
        mct_object <- mct_object$predicted_values
    }

    # classify is just the first n columns (before predicted.value)
    classify <- colnames(mct_object)[1]
    if(colnames(mct_object)[2] != "predicted.value") {
        classify2 <- colnames(mct_object)[2]
    }
    if(colnames(mct_object)[2] != "predicted.value" & colnames(mct_object)[3] != "predicted.value") {
        classify3 <- colnames(mct_object)[3]
    }

    # Get ylab as attribute
    ylab <- attributes(mct_object)$ylab

    yval <- ifelse("PredictedValue" %in% colnames(mct_object), "PredictedValue", "predicted.value")

    plot <- ggplot2::ggplot(data = mct_object, ggplot2::aes_(x = as.name(classify))) +
        ggplot2::geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
        ggplot2::geom_text(ggplot2::aes_(x = as.name(classify), y = mct_object$up, label = mct_object$groups), vjust = 0, nudge_y = (mct_object$up-mct_object$low)*label_height) +
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
autoplot.design <- function(design_object) {
    plot(design_object)
}

