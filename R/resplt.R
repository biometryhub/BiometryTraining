resplt <- function(mod.obj){
    aa <- data.frame(residuals = resid(mod.obj), fitted = fitted(mod.obj))

    aa$stdres <- aa$residuals/(sd(aa$residuals, na.rm = TRUE)*sqrt((length(!is.na(aa$residuals)-1))/(length(!is.na(aa$residuals)))))

    a <-  ggplot(data = aa, mapping = aes(x = stdres)) +
        geom_histogram(bins = ifelse(nrow(aa) < 31, 7, 11), fill = "aquamarine3", colour = "black") +
        theme_bw() +   labs(y = "Frequency", x = "Standardised Residual")


    qqplot.data <- function (vec = aa$stdres)
    {
        y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
        x <- qnorm(c(0.25, 0.75))
        slope <- 1
        int <- 0

        vecdf <- data.frame(resids = vec)

        ggplot(vecdf, aes(sample = resids)) + stat_qq(colour = "black", fill = "aquamarine3", size = 2 , shape = 21) +
            geom_abline(slope = slope, intercept = int) + theme_bw() +
            labs(y = "Standardised Residual", x = "Theoretical")
    }

    b <- qqplot.data(aa$stdres)


    c <- ggplot(data = aa, mapping = aes(x = fitted, y = stdres)) +
        geom_point(colour = "black", fill = "aquamarine3", size = 2 , shape = 21) + theme_bw() +
        labs(y = "Standardised Residual", x = "Fitted Value")



    library(gridExtra)
    library(ggpubr)

    ggarrange(a,b,c, labels = c("A", "B", "C"),
              nrow = 2, ncol = 2)


}
