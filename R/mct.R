#' Multiple Comparison Tests
#'
#' A function for comparing and ranking predicted means with Tukey's Honest Significant Difference (HSD) Test.
#'
#' @param model.obj An ASReml-R or aov model object.
#' @param pred.obj An ASReml-R prediction object with `sed = TRUE`.
#' @param sig The significance level, numeric between 0 and 1. Default is 0.05.
#' @param pred Name of predictor variable as string.
#' @param int.type The type of confidence interval to calculate. One of `ci`, `1se` or `2se`. Default is `ci`.
#' @param trans Transformation that was applied to the response variable. One of `log`, `sqrt`, `logit` or `inverse`. Default is `NA`.
#' @param offset Numeric offset applied to response variable prior to transformation. Default is `NA`.
#'
#' @importFrom multcompView multcompLetters
#' @importFrom agricolae LSD.test HSD.test
#' @importFrom stats predict
#'
#' @return A list containing a data frame `pred.tab` consisting of predicted means, standard errors, confidence interval upper and lower bounds, and significant group allocations.
#'
#' @examples
#' \dontrun{
#' library(asreml)
#'
#' #Fit ASreml Model
#' model.asr <- asreml(yield ~ Nitrogen + Variety + Nitrogen:Variety,
#'                     random = ~ Blocks + Blocks:Wplots,
#'                     residual = ~ units,
#'                     data = asreml::oats)
#'
#' wald(model.asr) #Nitrogen main effect significant
#'
#' #Calculate predicted means
#' pred.asr <- predict(model.asr, classify = "Nitrogen", sed = TRUE)
#'
#' #Determine ranking and groups according to Tukey's Test
#' tuk.rank <- mct.out(model.obj = model.asr, pred.obj = pred.asr, sig = 0.05,
#'                     int.type = "ci", pred = "Nitrogen")
#'
#' tuk.rank}
#'
#' @export
#'
mct.out <- function(model.obj, pred.obj, sig = 0.05, pred, int.type = "ci", trans = NA, offset = NA){

  if(class(model.obj)[1] == "asreml"){

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
    crit.val <- 1/sqrt(2)* qtukey((1-sig), nrow(pp), model.obj$nedf)*SED


    # Determine pairs that are significantly different
    diffs <- abs(outer(Mean, Mean,"-")) > crit.val
    diffs <- diffs[lower.tri(diffs)]

    # Create a vector of treatment comparison names
    m <- outer(pp$Names, pp$Names, paste, sep="-")
    m <- m[lower.tri(m)]


    names(diffs) <- m


    ll <- multcompView::multcompLetters(diffs, threshold = sig, compare = ">", reversed = TRUE)

    rr <- data.frame(groups = ll$Letters)
    rr$Names <- row.names(rr)


    pp.tab <- merge(pp,rr)


    if(!is.na(trans)){

      if(trans == "log"){
        pp.tab$PredictedValue <- exp(pp.tab$predicted.value) - ifelse(!is.na(offset), offset, 0)
        pp.tab$ApproxSE <- abs(pp.tab$std.error)*pp.tab$PredictedValue
        if(int.type == "ci"){
        pp.tab$ci <- qt(p = sig, model.obj$nedf, lower.tail = FALSE) * pp.tab$std.error
        }
        if(int.type == "1se"){
          pp.tab$ci <- pp.tab$std.error
        }
        if(int.type == "2se"){
          pp.tab$ci <- 2*pp.tab$std.error
        }
        pp.tab$low <- exp(pp.tab$predicted.value - pp.tab$ci) - ifelse(!is.na(offset), offset, 0)
        pp.tab$up <- exp(pp.tab$predicted.value + pp.tab$ci) - ifelse(!is.na(offset), offset, 0)
      }

      if(trans == "sqrt"){
        pp.tab$PredictedValue <- (pp.tab$predicted.value)^2 - ifelse(!is.na(offset), offset, 0)
        pp.tab$ApproxSE <- 2*abs(pp.tab$std.error)*sqrt(pp.tab$PredictedValue)
        if(int.type == "ci"){
          pp.tab$ci <- qt(p = sig, model.obj$nedf, lower.tail = FALSE) * pp.tab$std.error
        }
        if(int.type == "1se"){
          pp.tab$ci <- pp.tab$std.error
        }
        if(int.type == "2se"){
          pp.tab$ci <- 2*pp.tab$std.error
        }
        pp.tab$low <- (pp.tab$predicted.value - pp.tab$ci)^2 - ifelse(!is.na(offset), offset, 0)
        pp.tab$up <- (pp.tab$predicted.value + pp.tab$ci)^2 - ifelse(!is.na(offset), offset, 0)
      }


      if(trans == "logit"){
        pp.tab$PredictedValue <- exp(pp.tab$predicted.value)/(1 + exp(pp.tab$predicted.value))
        pp.tab$ApproxSE <- pp.tab$PredictedValue * (1 - pp.tab$PredictedValue)* abs(pp.tab$std.error)
        if(int.type == "ci"){
          pp.tab$ci <- qt(p = sig, model.obj$nedf, lower.tail = FALSE) * pp.tab$std.error
        }
        if(int.type == "1se"){
          pp.tab$ci <- pp.tab$std.error
        }
        if(int.type == "2se"){
          pp.tab$ci <- 2*pp.tab$std.error
        }
        pp.tab$ll <- pp.tab$predicted.value - pp.tab$ci
        pp.tab$low <- exp(pp.tab$ll)/(1 + exp(pp.tab$ll))
        pp.tab$uu <- pp.tab$predicted.value + pp.tab$ci
        pp.tab$up <- exp(pp.tab$uu)/(1 + exp(pp.tab$uu))

        pp.tab$ll <- NULL
        pp.tab$uu <- NULL
        pp.tab$transformed.value <- NULL
        pp.tab$approx.se <- NULL

      }

      if(trans == "inverse"){
        pp.tab$PredictedValue <- 1/pp.tab$predicted.value
        pp.tab$ApproxSE <- abs(pp.tab$std.error)*pp.tab$PredictedValue^2
        if(int.type == "ci"){
          pp.tab$ci <- qt(p = sig, model.obj$nedf, lower.tail = FALSE) * pp.tab$std.error
        }
        if(int.type == "1se"){
          pp.tab$ci <- pp.tab$std.error
        }
        if(int.type == "2se"){
          pp.tab$ci <- 2*pp.tab$std.error
        }
        pp.tab$low <- 1/(pp.tab$predicted.value - pp.tab$ci)
        pp.tab$up <- 1/(pp.tab$predicted.value + pp.tab$ci)
      }
    } else {

      if(int.type == "ci"){
        pp.tab$ci <- qt(p = sig, model.obj$nedf, lower.tail = FALSE) * pp.tab$std.error
      }
      if(int.type == "1se"){
        pp.tab$ci <- pp.tab$std.error
      }
      if(int.type == "2se"){
        pp.tab$ci <- 2*pp.tab$std.error
      }
      pp.tab$low <- pp.tab$predicted.value - pp.tab$ci
      pp.tab$up <- pp.tab$predicted.value + pp.tab$ci

    }
  }
  else {


    hsd.out <- agricolae::HSD.test(model.obj, trt = pred)

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
    if(int.type == "ci"){
      pp.tab$ci <- qt(p = sig, model.obj$nedf, lower.tail = FALSE) * pp.tab$std.error
    }
    if(int.type == "1se"){
      pp.tab$ci <- pp.tab$std.error
    }
    if(int.type == "2se"){
      pp.tab$ci <- 2*pp.tab$std.error
    }
    pp.tab$low <- pp.tab$predicted.value - pp.tab$ci
    pp.tab$up <- pp.tab$predicted.value + pp.tab$ci


  }

  if(!is.na(trans)){

    if(trans == "log"){
      pp.tab$PredictedValue <- exp(pp.tab$predicted.value) - ifelse(!is.na(offset), offset, 0)
      pp.tab$ApproxSE <- abs(pp.tab$std.error)*pp.tab$PredictedValue
      if(int.type == "ci"){
        pp.tab$ci <- qt(p = sig, model.obj$nedf, lower.tail = FALSE) * pp.tab$std.error
      }
      if(int.type == "1se"){
        pp.tab$ci <- pp.tab$std.error
      }
      if(int.type == "2se"){
        pp.tab$ci <- 2*pp.tab$std.error
      }
      pp.tab$low <- exp(pp.tab$predicted.value - pp.tab$ci) - ifelse(!is.na(offset), offset, 0)
      pp.tab$up <- exp(pp.tab$predicted.value + pp.tab$ci) - ifelse(!is.na(offset), offset, 0)
    }

    if(trans == "sqrt"){
      pp.tab$PredictedValue <- (pp.tab$predicted.value)^2 - ifelse(!is.na(offset), offset, 0)
      pp.tab$ApproxSE <- 2*abs(pp.tab$std.error)*sqrt(pp.tab$PredictedValue)
      if(int.type == "ci"){
        pp.tab$ci <- qt(p = sig, model.obj$nedf, lower.tail = FALSE) * pp.tab$std.error
      }
      if(int.type == "1se"){
        pp.tab$ci <- pp.tab$std.error
      }
      if(int.type == "2se"){
        pp.tab$ci <- 2*pp.tab$std.error
      }
      pp.tab$low <- (pp.tab$predicted.value - pp.tab$ci)^2 - ifelse(!is.na(offset), offset, 0)
      pp.tab$up <- (pp.tab$predicted.value + pp.tab$ci)^2 - ifelse(!is.na(offset), offset, 0)
    }


    if(trans == "logit"){
      pp.tab$PredictedValue <- exp(pp.tab$predicted.value)/(1 + exp(pp.tab$predicted.value))
      pp.tab$ApproxSE <- pp.tab$PredictedValue * (1 - pp.tab$PredictedValue)* abs(pp.tab$std.error)
      if(int.type == "ci"){
        pp.tab$ci <- qt(p = sig, model.obj$nedf, lower.tail = FALSE) * pp.tab$std.error
      }
      if(int.type == "1se"){
        pp.tab$ci <- pp.tab$std.error
      }
      if(int.type == "2se"){
        pp.tab$ci <- 2*pp.tab$std.error
      }
      pp.tab$ll <- pp.tab$predicted.value - pp.tab$ci
      pp.tab$low <- exp(pp.tab$ll)/(1 + exp(pp.tab$ll))
      pp.tab$uu <- pp.tab$predicted.value + pp.tab$ci
      pp.tab$up <- exp(pp.tab$uu)/(1 + exp(pp.tab$uu))

      pp.tab$ll <- NULL
      pp.tab$uu <- NULL
      pp.tab$transformed.value <- NULL
      pp.tab$approx.se <- NULL
    }

    if(trans == "inverse"){
      pp.tab$PredictedValue <- 1/pp.tab$predicted.value
      pp.tab$ApproxSE <- abs(pp.tab$std.error)*pp.tab$PredictedValue^2
      if(int.type == "ci"){
        pp.tab$ci <- qt(p = sig, model.obj$nedf, lower.tail = FALSE) * pp.tab$std.error
      }
      if(int.type == "1se"){
        pp.tab$ci <- pp.tab$std.error
      }
      if(int.type == "2se"){
        pp.tab$ci <- 2*pp.tab$std.error
      }
      pp.tab$low <- 1/(pp.tab$predicted.value - pp.tab$ci)
      pp.tab$up <- 1/(pp.tab$predicted.value + pp.tab$ci)
    }

  } else {

    if(int.type == "ci"){
      pp.tab$ci <- qt(p = sig, model.obj$nedf, lower.tail = FALSE) * pp.tab$std.error
    }
    if(int.type == "1se"){
      pp.tab$ci <- pp.tab$std.error
    }
    if(int.type == "2se"){
      pp.tab$ci <- 2*pp.tab$std.error
    }
    pp.tab$low <- pp.tab$predicted.value - pp.tab$ci
    pp.tab$up <- pp.tab$predicted.value + pp.tab$ci

  }

  pp.tab$Names <- NULL



  return(pp.tab)

}
