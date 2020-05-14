#' Multiple Comparison Tests
#'
#' A function for comparing and ranking predicted means with Tukey's Honest Significant Difference (HSD) Test.
#'
#' @param model.obj An ASReml-R model object.
#' @param pred.obj An ASReml-R prediction object with `sed=TRUE`.
#' @param sig The confidence level, numeric between 0 and 1. Default is 0.95.
#' @param pred Name of predictor variable as string.
#' @param typeR Type of test as string.
#' @param trans Transformation that was applied to the response variable before modelling, e.g. `log` or `sqrt`. Default is `NA`.
#' @param offset Numeric offset applied to response variable prior to transformation. Default is `NA`.
#'
#' @importFrom multcompView multcompLetters
#'
#' @return A list containing a data frame "pred.tab" consisting of predicted means, standard errors, confidence interval upper and lower bounds, and significant group allocations.
#'
#' @examples
#' \dontrun{library(asreml)
#' data(oats)
#'
#' #Fit ASreml Model
#' model.asr <- asreml(yield ~ Nitrogen*Variety,
#'                     random =~Blocks + Blocks:Wplots,
#'                     residual =~units,
#'                     data=oats)
#'
#' wald(model.asr) #Nitrogen main effect significant
#'
#' #Calculate predicted means
#' pred.asr <- predict(model.asr, classify="Nitrogen",sed = TRUE)
#'
#' #Determine ranking and groups according to Tukey's Test
#' tuk.rank <- mct.out(model.obj = model.asr, pred.obj = pred.asr, sig = 0.95, pred = "Nitrogen", typeR = "tukey")
#'
#' tuk.rank}
#'
#' @export
#'
mct.out <- function(model.obj, pred.obj, sig = 0.95, pred, typeR, trans = NA, offset = NA){

  if(class(model.obj)[1] == "asreml"){

    avelsd <- qt(1-sig/2, model.obj$nedf) * pred.obj$avsed[names(pred.obj$avsed) == "mean"]

    # Can we get the pred argument directly from the model.obj? - NO

    #For use with asreml 4+
    if(packageVersion("asreml") > 4) {
      pp <- pred.obj$pvals
      sed <- pred.obj$sed
    }

    pp <- pp[!is.na(pp$predicted.value),]
    pp$status <- NULL

    ifelse(grepl(":", pred),
           pp$Names <- apply(pp[,unlist(strsplit(pred, ":"))], 1, paste, collapse = "_"),
           pp$Names <- pp[[pred]])

    zz <- as.numeric(row.names(pp[!is.na(pp$predicted.value),]))

    SED <- sed[zz,zz]
    Mean <- pp$predicted.value
    Names <-  as.character(pp$Names)
    if(typeR == "tukey"){
      crit.val <- 1/sqrt(2)* qtukey((1-sig), nrow(pp), model.obj$nedf)*SED
    } else
    { crit.val <- qt((1-sig/2), model.obj$nedf)*SED
    }


    # Determine pairs that are significantly different
    diffs <- abs(outer(Mean, Mean,"-")) > crit.val
    diffs <- diffs[lower.tri(diffs)]

    # Create a vector of treatment comparison names
    m <- outer(pp$Names, pp$Names, paste, sep="-")
    m <- m[lower.tri(m)]


    names(diffs) <- m

    if(!require(multcompView)){
      install.packages("multcompView")
    }
    library(multcompView)


    ll <- multcompLetters(diffs, threshold = sig, compare = ">", reversed = TRUE)

    rr <- data.frame(ll$Letters)
    rr$Names <- row.names(rr)
    names(rr)[1] <- paste("groups", typeR, sep = "_")

    pp$ci <- qt(p = (1-sig/2), model.obj$nedf) * pp$std.error
    pp$low <- pp$predicted.value - pp$ci
    pp$up <- pp$predicted.value + pp$ci

    pp.tab <- merge(pp,rr)


    if(!is.na(trans)){

      if(trans == "log"){
        pp.tab$PredictedValue <- exp(pp.tab$predicted.value) - offset
        pp.tab$ApproxSE <- abs(pp.tab$std.error)*pp.tab$PredictedValue
        pp.tab$ci <- qt(p = (1-sig/2), model.obj$nedf) * pp.tab$std.error
        pp.tab$low <- exp(pp.tab$predicted.value - pp.tab$ci) - offset
        pp.tab$up <- exp(pp.tab$predicted.value + pp.tab$ci) - offset
      }

      if(trans == "sqrt"){
        pp.tab$PredictedValue <- (pp.tab$predicted.value)^2 - offset
        pp.tab$ApproxSE <- 2*abs(pp.tab$std.error)*pp.tab$PredictedValue
        pp.tab$ci <- qt(p = (1-sig/2), model.obj$nedf) * pp.tab$std.error
        pp.tab$low <- (pp.tab$predicted.value - pp.tab$ci)^2 - offset
        pp.tab$up <- (pp.tab$predicted.value + pp.tab$ci)^2 - offset
      }
    }

    out.list <- list()

    out.list$pred.tab <- pp.tab
    out.list$ave.LSD <- avelsd

  }

  else {

    if(typeR == "LSD"){

      lsd.out <- LSD.test(model.obj, trt = pred)

      hh <- lsd.out$groups
      hh[[pred]] <- row.names(hh)
      hh[[model.obj$terms[[2]]]] <- NULL

      aa <- data.frame(X = model.obj$xlevels[[names(model.obj$xlevels)]])
      names(aa) <- names(model.obj$xlevels)
      pp.tab <- predict(model.obj, aa, se.fit = TRUE)
      aa$predicted.value <- pp.tab$fit
      aa$std.error <- pp.tab$se.fit

      pp.tab <- merge(aa, hh)

      model.obj$nedf <-  model.obj$df.residual
      pp.tab$ci <- qt(p = (1-sig/2), model.obj$nedf) * pp.tab$std.error
      pp.tab$low <- pp.tab$predicted.value - pp.tab$ci
      pp.tab$up <- pp.tab$predicted.value + pp.tab$ci




    }

    if(typeR == "tukey"){

      hsd.out <- HSD.test(model.obj, trt = pred)

      hh <- hsd.out$groups
      hh[[pred]] <- row.names(hh)
      hh[[model.obj$terms[[2]]]] <- NULL

      aa <- data.frame(X = model.obj$xlevels[[names(model.obj$xlevels)]])
      names(aa) <- names(model.obj$xlevels)
      pp.tab <- predict(model.obj, aa, se.fit = TRUE)
      aa$predicted.value <- pp.tab$fit
      aa$std.error <- pp.tab$se.fit

      pp.tab <- merge(aa, hh)
      model.obj$nedf <-  model.obj$df.residual
      pp.tab$ci <- qt(p = (1-sig/2), model.obj$df.residual) * pp.tab$std.error
      pp.tab$low <- pp.tab$predicted.value - pp.tab$ci
      pp.tab$up <- pp.tab$predicted.value + pp.tab$ci
    }

  }

  if(!is.na(trans)){

    if(trans == "log"){
      pp.tab$PredictedValue <- exp(pp.tab$predicted.value) - offset
      pp.tab$ApproxSE <- abs(pp.tab$std.error)*pp.tab$PredictedValue
      pp.tab$ci <- qt(p = (1-sig/2), model.obj$nedf) * pp.tab$std.error
      pp.tab$low <- exp(pp.tab$predicted.value - pp.tab$ci) - offset
      pp.tab$up <- exp(pp.tab$predicted.value + pp.tab$ci) - offset
    }

    if(trans == "sqrt"){
      pp.tab$PredictedValue <- (pp.tab$predicted.value)^2 - offset
      pp.tab$ApproxSE <- 2*abs(pp.tab$std.error)*pp.tab$PredictedValue
      pp.tab$ci <- qt(p = (1-sig/2), model.obj$nedf) * pp.tab$std.error
      pp.tab$low <- (pp.tab$predicted.value - pp.tab$ci)^2 - offset
      pp.tab$up <- (pp.tab$predicted.value + pp.tab$ci)^2 - offset
    }
  }
  out.list <- list()

  out.list$pred.tab <- pp.tab

  return(out.list)

}
