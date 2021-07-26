#' Variogram for spatial models.
#'
#' Produces variogram plots for checking spatial trends.
#'
#' @param mod.obj An `asreml` model object.
#'
#' @return A list containing ggplot2 objects.
#'
#'
#' @examples
#' dat.asr <- asreml(Petal.Length ~ Petal.Width, data = iris)
#' vario(dat.asr)
#' @export

vario <- function(mod.obj){

    if(!("asreml" %in% class(mod.obj))) {
        stop("mod.obj must be an asreml object")
    }

    aa <- asreml::varioGram(mod.obj)
    xnam <- names(aa)[2]
    ynam <- names(aa)[1]
    fld <- akima::interp(y = aa[,1], x = aa[,2], z = aa$gamma)
    gdat <- akima::interp2xyz(fld, data.frame = TRUE)

        a <- ggplot2::ggplot(gdat, ggplot2::aes(x = y, y = x, z = z, fill = z)) +
        ggplot2::geom_tile(alpha = 0.6) +
        ggplot2::coord_equal() +
        ggplot2::geom_contour(color = "white", alpha = 0.5) +
        ggplot2::scale_fill_gradientn(colours = rainbow(100)) +
        ggplot2::theme_bw(base_size = 6) +
        ggplot2::scale_y_continuous(expand = c(0, 0), breaks = seq(1, max(gdat$x), 2)) +
        ggplot2::scale_x_continuous(expand = c(0, 0), breaks = seq(1, max(gdat$y), 2)) +
        ggplot2::theme(legend.position = "none", aspect.ratio = 0.3) +
        ggplot2::labs(y = paste(xnam, "Lag", sep = " "), x = paste(ynam, "Lag", sep = " "))


        b <- lattice::wireframe(z ~ y * x, data = gdat, aspect = c(61/87, 0.4),
                                scales = list(cex = 0.5, arrows = FALSE),
                                shade = TRUE, colorkey = FALSE,
                                par.settings = list(axis.line = list(col = 'transparent')),
                                xlab = list(label = paste(ynam, "Lag", sep = " "), cex = .8, rot = 20),
                                ylab = list(label = paste(xnam, "Lag", sep = " "), cex = .8, rot = -18),
                                zlab = list(label = NULL, cex.axis = 0.5))


        #b <- latticeExtra::resizePanels(update(b, aspect  = 0.75,
        #                                           par.settings = list(fontsize = list(text = 8))))

        #b <- ggplotify::as.grob(b)

        output <- cowplot::plot_grid(b, a, nrow = 2, labels = c('A', 'B'), scale = c(2, 1))

        output

        return(output)
}

#' @rdname vario
#' @export
