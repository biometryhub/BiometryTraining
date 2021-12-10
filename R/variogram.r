#' Variogram plots for spatial models.
#'
#' Produces variogram plots for checking spatial trends.
#'
#' @param model.obj An `asreml` model object.
#'
#' @return A list containing ggplot2 objects.
#'
#' @importFrom akima interp interp2xyz
#' @importFrom grDevices rainbow
#' @importFrom lattice wireframe
#' @importFrom cowplot plot_grid
#' @importFrom ggplot2 ggplot geom_tile coord_equal geom_contour scale_fill_gradientn theme_bw scale_x_continuous scale_y_continuous theme labs
#'
#' @references S. P. Kaluzny, S. C. Vega, T. P. Cardoso, A. A. Shelly, _S+SpatialStats: User’s Manual for Windows® and UNIX®_ (Springer New York, 2013; https://books.google.com.au/books?id=iADkBwAAQBAJ).
#'
#' @examples
#' \dontrun{
#' library(asreml)
#' oats <- asreml::oats
#' oats <- oats[order(oats$Row, oats$Column),]
#' model.asr <- asreml(yield ~ Nitrogen + Variety + Nitrogen:Variety,
#'                     random = ~ Blocks + Blocks:Wplots,
#'                     residual = ~ ar1(Row):ar1(Column),
#'                     data = oats)
#' variogram(model.asr)
#' }
#' @export

variogram <- function(model.obj, Row, Col){

    if(!(inherits(model.obj, "asreml"))) {
        stop("model.obj must be an asreml model object")
    }

    x <- NULL
    y <- NULL
    z <- NULL

    aa <- variogram(model.obj)
    xnam <- names(aa)[2]
    ynam <- names(aa)[1]
    fld <- akima::interp(y = aa[,1], x = aa[,2], z = aa$gamma)
    gdat <- akima::interp2xyz(fld, data.frame = TRUE)

    a <- ggplot2::ggplot(gdat, ggplot2::aes(x = y, y = x, z = z, fill = z)) +
        ggplot2::geom_tile(alpha = 0.6) +
        ggplot2::coord_equal() +
        ggplot2::geom_contour(color = "white", alpha = 0.5) +
        ggplot2::scale_fill_gradientn(colours = grDevices::rainbow(100)) +
        ggplot2::theme_bw(base_size = 8) +
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

    output <- cowplot::plot_grid(b, a, nrow = 2, scale = c(2, 1))

    return(output)
}

#' Calculate the variogram data frame for a model
#'
#' @param model.obj An asreml model
#'
#' @return A data frame with the variogram for a model. The data frame contains the spatial coordinaties (typically row and column), the $gamma$ for that position and the number of points with the separation.
#' @keywords internal
#'
#'
#' @examples
#' \dontrun{
#' library(asreml)
#' oats <- asreml::oats
#' oats <- oats[order(oats$Row, oats$Column),]
#' model.asr <- asreml(yield ~ Nitrogen + Variety + Nitrogen:Variety,
#'                     random = ~ Blocks + Blocks:Wplots,
#'                     residual = ~ ar1(Row):ar1(Column),
#'                     data = oats)
#' vario_df(model.asr)
#' }
#'
vario_df <- function(model.obj, Row, Col) {
    # So the 'z' value for the variogram is the residuals
    # Need to be able to pull out the x/y from the model object

    dims <- unlist(strsplit(names(model.obj$R.param[1]), ":"))
    # vals <- cbind(data.frame(model.obj$mf[[dims[1]]]), data.frame(model.obj$mf[[dims[2]]]), resid = resid(model.obj))
    # colnames(vals) <- c(dims, "resid")
    Row <- as.numeric(model.obj$mf[[dims[1]]])
    Column <- as.numeric(model.obj$mf[[dims[2]]])
    Resid <- residuals(model.obj)

    nrows <- max(Row)
    ncols <- max(Column)

    vario <- expand.grid(Row = 0:(nrows-1), Column = 0:(ncols-1))

    # Ignore the 0, 0 case (gamma=0, counted row*cols times)
    gammas <- rep(0, nrows*ncols)
    nps <- rep(nrows*ncols, nrows*ncols)

    for (index in 2:nrow(vario)) {
        i <- vario[index, 'Row']
        j <- vario[index, 'Column']

        gamma <- 0
        np <- 0
        for (val_index in 1:nrows) {
            # val <- vals[val_index, ]

            # Deliberate double-counting so that offset handling is easy
            # (so e.g. we compute distance from (1,1)->(2,3), and then again
            # later from (2,3)->(1,1)).
            for (offset in unique(list(c(i, j), c(-i, j), c(i, -j), c(-i, -j)))) {
                row <- Row[val_index] + offset[1]
                col <- Column[val_index] + offset[2]

                if (0 < row && row <= nrows && 0 < col && col <= ncols) {
                    other <- which(Row == row & Column == col)
                    gamma <- gamma + (Resid[val_index]-Resid[other])^2
                    np <- np + 1
                }
            }
        }
        # Since we double-counted precisely, halve to get the correct answer.
        np <- np / 2
        gamma <- gamma / 2

        if (np > 0) {
            gamma <- gamma / (2*np)
        }

        gammas[index] <- gamma
        nps[index] <- np
    }
    vario <- cbind(vario, data.frame(gamma = gammas, np = nps))
    colnames(vario) <- c(dims, "gamma", "np")
    class(vario) <- c("varioGram", "data.frame")
    return(vario)
}
