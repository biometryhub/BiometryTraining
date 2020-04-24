#' Tukey's Honest Significant Difference test
#'
#' Performs Tukey's Honest Significant Difference test and optionally back-transforms natural log and squareroot transformed predicted values.
#'
#'
#' @param pred.obj An ASReml-R prediction object.
#' @param model.obj An ASReml-R model object.
#' @param pred A character string of the main effect or interaction terms predicted from the model.
#' @param sig The significance level to test at.
#' @param trans Choice of `log` or `sqrt` to back-transform predicted values and standard errors in the output.
#' @param offset A numeric value used in the transformation.
#'
#' @return A data frame with the predicted values and results of Tukey's test.
#'
#' @importFrom stats qt qtukey
#' @importFrom utils packageVersion
#'
#' @references <http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.47.9023>
#'
#' @examples
#' \dontrun{dat.asr <- asreml(Yield ~ Variety, random = ~ Block,
#' residual = ~ id(Plot), data = dat)
#'
#' dat.pred <- predict(dat.asr, classify = "Variety",
#'                     sed = TRUE)
#'
#' pred.out <- tuk.out(model.obj = dat.asr, pred.obj = dat.pred,
#'                    data = dat, pred = "Variety", sig = 0.95)
#'
#' pred.out}
#'
#' @export
tuk.out <- function(pred.obj, model.obj, pred, sig = 0.95, trans = FALSE, offset = 0){

    # Can we get the pred argument directly from the model.obj?

    #For use with asreml 4+
    if(packageVersion("asreml")>4) {
        pp <- pred.obj$pvals
        sed <- pred.obj$sed
    }
    #For use with asreml 3.0
    else {
        pp <- pred.obj$predictions$`pvals`
        sed <- pred.obj$predictions$sed
    }

    pp <- pp[!is.na(pp$predicted.value),]
    pp$status <- NULL

    ifelse(grep(":", pred),
           pp$Names <- apply(pp[,unlist(strsplit(pred, ":"))], 1, paste, collapse = "-"),
           pp$Names <- pp[[pred]])

    zz <- as.numeric(row.names(pp[!is.na(pp$predicted.value),]))
    dat.tuk <- tukey.rank(
        Mean = pp$predicted.value,
        Names = as.character(pp$Names),
        SED = sed[zz,zz],
        crit.val = 1/sqrt(2)*qtukey(sig, nrow(pp), model.obj$nedf))
    dat.tuk <- dat.tuk[order(dat.tuk$Names),]
    ###### To strip the white spaces ########
    dat.tuk$groups <- as.character(dat.tuk$groups)
    for(j in 1:length(dat.tuk$groups)){
        aa <- dat.tuk$groups[j]
        bb <- gsub(" ","", aa, fixed=TRUE)
        dat.tuk$groups[j] <- bb
    }
    names(dat.tuk)[2] <- "predicted.value"
    pp <- merge(pp, dat.tuk)
    pp$Names <- NULL

    pp$ci <- qt(p = 1-(1-sig)/2, model.obj$nedf) * pp$std.error
    pp$low <- pp$predicted.value - pp$ci
    pp$up <- pp$predicted.value + pp$ci

    if(trans == "log"){
        pp$PredictedValue <- exp(pp$predicted.value) - offset
        pp$ApproxSE <- abs(pp$std.error)*pp$PredictedValue
        pp$ci <- qt(p = 1-(1-sig)/2, model.obj$nedf) * pp$std.error
        pp$low <- exp(pp$predicted.value - pp$ci) - offset
        pp$up <- exp(pp$predicted.value + pp$ci) - offset
    }

    if(trans == "sqrt"){
        pp$PredictedValue <- (pp$predicted.value)^2 - offset
        pp$ApproxSE <- 2*abs(pp$std.error)*pp$PredictedValue
        pp$ci <- qt(p = 1-(1-sig)/2, model.obj$nedf) * pp$std.error
        pp$low <- (pp$predicted.value - pp$ci)^2 - offset
        pp$up <- (pp$predicted.value + pp$ci)^2 - offset
    }

    return(pp)
}

