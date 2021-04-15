#' Residual plots of aov or ASreml-R models.
#'
#' Produces plots of residuals for assumption checking of an ANOVA or asremlR model.
#'
#' @param mod.obj An `aov` or `asreml` model object.
#' @param shapiro (Logical) Display the Shapiro-Wilks test of normality on the plot?
#'
#' @return A list containing ggplot2 objects which are diagnostic plots.
#'
#' @importFrom ggplot2 ggplot geom_histogram aes theme_bw stat_qq labs geom_qq_line geom_point
#' @importFrom stats fitted qnorm quantile resid sd
#' @importFrom patchwork wrap_plots plot_annotation
#' @importFrom grid gpar
#' @importFrom gridtext richtext_grob
#' @importFrom stats shapiro.test
#'
#' @aliases resplot
#'
#' @examples
#' dat.aov <- aov(Petal.Length ~ Petal.Width, data = iris)
#' resplt(dat.aov)
#' @export

resplt <- function(mod.obj, shapiro = T){

    # Assign NULL to variables that give a NOTE in package checks
    # Known issue. See https://www.r-bloggers.com/no-visible-binding-for-global-variable/

    stdres <- NULL
    resids <- NULL

    if(!("aov" %in% class(mod.obj)|"asreml" %in% class(mod.obj))) {
        stop("mod.obj must be an aov or asreml object")
    }

    if ("aov" %in% class(mod.obj)) {
        facet <- 1
        facet_name <- NULL
        k <- length(mod.obj$residual)
    }

    if ("asreml" %in% class(mod.obj)){
        facet <- length(names(mod.obj$R.param))
        if (facet > 1) {
            facet_name <- names(mod.obj$R.param)
            k <- unlist(lapply(1:facet, function(i) mod.obj$R.param[[i]]$variance$size))
        }
        else {
            facet_name <- NULL
            k <- length(mod.obj$residual)
        }
    }

    aa <- data.frame(residuals = resid(mod.obj), fitted = fitted(mod.obj), lvl = rep(1:facet, k))

    output <- list()

    for (i in 1:facet){

        aa.f <- aa[aa$lvl==i,]
        aa.f$stdres <- aa.f$residuals/(sd(aa.f$residuals, na.rm = TRUE)*sqrt((length(!is.na(aa.f$residuals)-1))/(length(!is.na(aa.f$residuals)))))

        a <- ggplot2::ggplot(data = aa.f, mapping = ggplot2::aes(x = stdres)) +
            ggplot2::geom_histogram(bins = ifelse(nrow(aa) < 31, 7, 11), fill = "aquamarine3", colour = "black") +
            ggplot2::theme_bw() + ggplot2::labs(y = "Frequency", x = "Standardised Residual")

        b <- ggplot2::ggplot(aa.f, ggplot2::aes(sample = stdres)) + ggplot2::geom_qq(colour = "black", fill = "aquamarine3", size = 2 , shape = 21) +
            ggplot2::geom_qq_line() + ggplot2::theme_bw() +
            ggplot2::labs(y = "Standardised Residual", x = "Theoretical")

        c <- ggplot2::ggplot(data = aa.f, mapping = ggplot2::aes(x = fitted, y = stdres)) +
            ggplot2::geom_point(colour = "black", fill = "aquamarine3", size = 2 , shape = 21) + ggplot2::theme_bw() +
            ggplot2::labs(y = "Standardised Residual", x = "Fitted Value")

        if(shapiro) {
            shap <- shapiro.test(aa.f$residuals)

            shapiro_text <- c(paste(shap$method, "p-value:", round(shap$p.value, 4)),
                              ifelse(shap$p.value>0.05,
                                     paste0("The residuals appear to be normally distributed. (n = ", length(aa.f$residuals), ")"),
                                     paste0("The residuals do not appear to be normally distributed. (n = ", length(aa.f$residuals), ")")))

            d <- gridtext::richtext_grob(shapiro_text, y = c(0.65, 0.4), gp = grid::gpar(fontsize = c(12, 10)))#,
            output[[i]] <- patchwork::wrap_plots(A = a, B = b, C = c, D = d, design = "AB\nAB\nAB\nAB\nCC\nCC\nCC\nCC\nDD", tag_level = "new") +
                patchwork::plot_annotation(tag_levels = list(c("A", "B", "C")),title = facet_name[i])
        }
        else{
            output[[i]] <- patchwork::wrap_plots(A = a, B = b, C = c, design = "AB\nCC", tag_level = "new") +
                patchwork::plot_annotation(tag_levels = "A",title = facet_name[i])
        }
    }

    if(facet>1) {
        names(output) <- facet_name
        return(output)
    }
    else {
        return(output[[1]])
    }
}

#' @rdname resplt
resplot <- resplt
