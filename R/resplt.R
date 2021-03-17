#' Produces plots of residuals for assumption checking of an ANOVA or asremlR model.
#'
#' @param mod.obj An `aov` model or `asreml`.
#'
#' @return A list containing ggplot2 objects which are diagnostic plots.
#'
#' @importFrom ggplot2 ggplot geom_histogram aes theme_bw stat_qq labs geom_abline geom_point
#' @importFrom stats fitted qnorm quantile resid sd
#' @importFrom patchwork wrap_plots plot_annotation
#'
#' @examples
#' dat.aov <- aov(Petal.Length ~ Petal.Width, data = iris)
#' resplt(dat.aov)
#' @export

resplt <- function(mod.obj){

    # Asign NULL to variables that give a NOTE in package checks
    # Known issue. See https://www.r-bloggers.com/no-visible-binding-for-global-variable/

    stdres <- NULL
    resids <- NULL

    if(!("aov" %in% class(mod.obj)|"asreml" %in% class(mod.obj))) {
        stop("mod.obj must be an aov or asreml object")
    }

    if ("aov" %in% class(mod.obj)) {
        facet <- 1
        facet_name <- NULL
    }

    if ("asreml" %in% class(mod.obj)){
        facet <- length(names(mod.obj$R.param))
        if (facet > 1) {
            facet_name <- names(mod.obj$R.param)
        }
        else
            facet_name <- NULL
    }

    k <- length(resid(mod.obj))/facet

    aa <- data.frame(residuals = resid(mod.obj), fitted = fitted(mod.obj), lvl = rep(1:facet,each=k))

    output <- list()

    for (i in 1:facet){

        aa.f <- aa[aa$lvl==i,]
        aa.f$stdres <- aa.f$residuals/(sd(aa.f$residuals, na.rm = TRUE)*sqrt((length(!is.na(aa.f$residuals)-1))/(length(!is.na(aa.f$residuals)))))


        a <- ggplot2::ggplot(data = aa.f, mapping = ggplot2::aes(x = stdres)) +
            ggplot2::geom_histogram(bins = ifelse(nrow(aa) < 31, 7, 11), fill = "aquamarine3", colour = "black") +
            ggplot2::theme_bw() + ggplot2::labs(y = "Frequency", x = "Standardised Residual")

        # qqplot.data <- function (vec = aa$stdres)
        # {
        #     y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
        #     x <- qnorm(c(0.25, 0.75))
        #     slope <- 1
        #     int <- 0
        #
        #     vecdf <- data.frame(resids = vec)
        #
        #     ggplot2::ggplot(vecdf, ggplot2::aes(sample = resids)) + ggplot2::stat_qq(colour = "black", fill = "aquamarine3", size = 2 , shape = 21) +
        #         ggplot2::geom_abline(slope = slope, intercept = int) + ggplot2::theme_bw() +
        #         ggplot2::labs(y = "Standardised Residual", x = "Theoretical")
        # }
        #
        # b <- qqplot.data(aa.f$stdres)

        b <- ggplot2::ggplot(aa.f, ggplot2::aes(sample = stdres)) + ggplot2::geom_qq(colour = "black", fill = "aquamarine3", size = 2 , shape = 21) +
            ggplot2::geom_qq_line() + ggplot2::theme_bw() +
            ggplot2::labs(y = "Standardised Residual", x = "Theoretical")


        c <- ggplot2::ggplot(data = aa.f, mapping = ggplot2::aes(x = fitted, y = stdres)) +
            ggplot2::geom_point(colour = "black", fill = "aquamarine3", size = 2 , shape = 21) + ggplot2::theme_bw() +
            ggplot2::labs(y = "Standardised Residual", x = "Fitted Value")

        # output <- ggpubr::ggarrange(a,b,c, labels = c("A", "B", "C"),
        # nrow = 2, ncol = 2)
        output[[i]] <- patchwork::wrap_plots(a, b, c, ncol = 2, nrow = 2, tag_level = "new") +
            patchwork::plot_annotation(tag_levels = "A",title = facet_name[i])
    }

    return(output)
}
