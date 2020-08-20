#' Multiple Comparison Tests
#'
#' A function for comparing and ranking predicted means with Tukey's Honest Significant Difference (HSD) Test.
#'
#' @param model.obj An ASReml-R or aov model object.
#' @param pred.obj An ASReml-R prediction object with `sed = TRUE`. Not required for aov models, so set to `NA`.
#' @param sig The significance level, numeric between 0 and 1. Default is 0.05.
#' @param pred Name of predictor variable as string.
#' @param int.type The type of confidence interval to calculate. One of `ci`, `1se` or `2se`. Default is `ci`.
#' @param trans Transformation that was applied to the response variable. One of `log`, `sqrt`, `logit` or `inverse`. Default is `NA`.
#' @param offset Numeric offset applied to response variable prior to transformation. Default is `NA`.
#' @param round Controls rounding of decimal places in output. Default is 2 decimal places.
#' @param order Order of the letters in the groups output. Options are `'ascending'` (the default), or `'descending'`. Alternative options that are accepted are `increasing` and `decreasing`. Partial matching of text is performed, allowing entry of `'desc'` for example.
#'
#' @importFrom multcompView multcompLetters
#' @importFrom agricolae LSD.test HSD.test
#' @importFrom predictmeans predictmeans
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
mct.out <- function(model.obj, pred.obj, sig = 0.05, pred, int.type = "ci", trans = NA, offset = NA, round = 2, order = "ascending"){

  if(class(model.obj)[1] == "asreml"){

    #For use with asreml 4+
    if(packageVersion("asreml") > 4) {
      pp <- pred.obj$pvals

      # Check that the prediction object was created with the sed matrix
      if(is.null(pred.obj$sed)) {
        stop("Prediction object (pred.obj) must be created with argument sed = TRUE.")
      }

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
    nedf <- model.obj$nedf
    crit.val <- 1/sqrt(2)* qtukey((1-sig), nrow(pp), nedf)*SED
  } else {


    pred.out <- predictmeans::predictmeans(model.obj, pred, mplot = FALSE)
    pred.out$mean_table <- pred.out$mean_table[,!grepl("95", names(pred.out$mean_table))]
    sed <- pred.out$`Standard Error of Differences`[1]
    pp <- pred.out$mean_table
    names(pp)[names(pp) == "Predicted means"] <- "predicted.value"
    names(pp)[names(pp) == "Standard error"] <- "std.error"

    SED <- matrix(data = sed, nrow = nrow(pp), ncol = nrow(pp))
    diag(SED) <- NA
    Mean <- pp$predicted.value
    ifelse(grepl(":", pred),
           pp$Names <- apply(pp[,unlist(strsplit(pred, ":"))], 1, paste, collapse = "_"),
           pp$Names <- pp[[pred]])

    Names <-  as.character(pp$Names)
    nedf <- pp$Df[1]
    crit.val <- 1/sqrt(2)* qtukey((1-sig), nrow(pp), nedf)*SED

  }



    # Determine pairs that are significantly different
    diffs <- abs(outer(Mean, Mean,"-")) > crit.val
    diffs <- diffs[lower.tri(diffs)]

    # Create a vector of treatment comparison names
    m <- outer(pp$Names, pp$Names, paste, sep="-")
    m <- m[lower.tri(m)]


    names(diffs) <- m

    # Check ordering of output
    # Refactor with switch cases?
    ordering <- match.arg(order, c('ascending', 'descending', 'increasing', 'decreasing'))

    if(ordering == "ascending" | ordering == "increasing") {
      # Set ordering to FALSE to set decreasing = F in order function
      ordering <- FALSE
    }
    else if(ordering == "descending" | ordering == "decreasing") {
      # Set ordering to TRUE to set decreasing = T in order function
      ordering <- TRUE
    }
    else {
      stop("order must be one of ascending, descending, increasing or decreasing")
    }

    ll <- multcompView::multcompLetters3("Names", "predicted.value", diffs, pp, reversed = ordering)

    rr <- data.frame(groups = ll$Letters)
    rr$Names <- row.names(rr)


    pp.tab <- merge(pp,rr)


    if(!is.na(trans)){

      if(trans == "log"){
        pp.tab$PredictedValue <- exp(pp.tab$predicted.value) - ifelse(!is.na(offset), offset, 0)
        pp.tab$ApproxSE <- abs(pp.tab$std.error)*pp.tab$PredictedValue
        if(int.type == "ci"){
        pp.tab$ci <- qt(p = sig, nedf, lower.tail = FALSE) * pp.tab$std.error
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
          pp.tab$ci <- qt(p = sig, nedf, lower.tail = FALSE) * pp.tab$std.error
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
          pp.tab$ci <- qt(p = sig, nedf, lower.tail = FALSE) * pp.tab$std.error
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
          pp.tab$ci <- qt(p = sig, nedf, lower.tail = FALSE) * pp.tab$std.error
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
        pp.tab$ci <- qt(p = sig, nedf, lower.tail = FALSE) * pp.tab$std.error
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

  # ordering <- match.arg(order, c('ascending', 'descending', 'increasing', 'decreasing'))
  #
  # if(ordering == "ascending" | ordering == "increasing") {
  #   # Set ordering to FALSE to set decreasing = F in order function
  #   ordering <- FALSE
  # }
  # else if(ordering == "descending" | ordering == "decreasing") {
  #   # Set ordering to TRUE to set decreasing = T in order function
  #   ordering <- TRUE
  # }
  # else {
  #   stop("order must be one of ascending, descending, increasing or decreasing")
  # }

  # pp.tab <- pp.tab[order(pp.tab$predicted.value, decreasing = ordering),]
  # pp.tab$groups <- pp.tab$groups[order(pp.tab$groups)]

  # pp.tab <- dplyr::mutate(pp.tab, dplyr::across(where(is.numeric), base::round, digits = round))
  pp.tab <- rapply(object = pp.tab, f = base::round, classes = "numeric", how = "replace", digits = round)

  return(pp.tab)
}
