#' Multiple Comparison Tests
#'
#' A function for comparing and ranking predicted means with Tukey's Honest Significant Difference (HSD) Test.
#'
#' @param model.obj An ASReml-R or aov model object.
#' @param pred.obj An ASReml-R prediction object with `sed = TRUE`. Not required for aov models, so set to `NA`.
#' @param classify Name of predictor variable as string.
#' @param sig The significance level, numeric between 0 and 1. Default is 0.05.
#' @param int.type The type of confidence interval to calculate. One of `ci`, `1se` or `2se`. Default is `ci`.
#' @param trans Transformation that was applied to the response variable. One of `log`, `sqrt`, `logit` or `inverse`. Default is `NA`.
#' @param offset Numeric offset applied to response variable prior to transformation. Default is `NA`. Use 0 if no offset was applied to the transformed data. See Details for more information.
#' @param decimals Controls rounding of decimal places in output. Default is 2 decimal places.
#' @param order Order of the letters in the groups output. Options are `'default'`, `'ascending'` or `'descending'`. Alternative options that are accepted are `increasing` and `decreasing`. Partial matching of text is performed, allowing entry of `'desc'` for example.
#' @param save Logical (default `FALSE`). Save the predicted values to a csv file?
#' @param savename A filename for the predicted values to be saved to. Default is `predicted_values`.
#' @param pred Deprecated. Use `classify` instead.
#'
#' @importFrom multcompView multcompLetters
#' @importFrom agricolae LSD.test HSD.test
#' @importFrom predictmeans predictmeans
#' @importFrom stats predict
#' @importFrom forcats fct_inorder
#' @importFrom ggplot2 ggplot aes_ aes geom_errorbar geom_text geom_point theme_bw lab
#'
#' @details Some transformations require that data has a small offset applied, otherwise it will cause errors (for example taking a log of 0, or square root of negative values). In order to correctly reverse this offset, if a the `trans` argument is supplied, an offset value must also be supplied. If there was no offset required for a transformation, then use a value of 0 for the `offset` argument.
#'
#' @return A list containing a data frame with predicted means, standard errors, confidence interval upper and lower bounds, and significant group allocations, as well as a plot visually displaying the predicted values.
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
#' pred.out <- mct.out(model.obj = model.asr, pred.obj = pred.asr, sig = 0.05,
#'                     int.type = "ci", classify = "Nitrogen", order = "descending", decimals = 5)
#'
#' pred.out}
#'
#' @export
#'
mct.out <- function(model.obj,
                    pred.obj,
                    classify,
                    sig = 0.05,
                    int.type = "ci",
                    trans = NA,
                    offset = NA,
                    decimals = 2,
                    order = "default",
                    save = FALSE,
                    savename = "predicted_values",
                    plottype = "pdf",
                    pred){

  if(!missing(pred)) {
    warning("Argument pred has been deprecated and will be removed in a future version. Please use classify instead.")
    classify <- pred
  }

  if(class(model.obj)[1] == "asreml"){

    if(missing(pred.obj)) {
      stop("You must provide a prediction object in pred.obj")
    }

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

    dat.ww <- asreml::wald(model.obj, ssType = "conditional", denDF = "default", trace = FALSE)$Wald

    dendf <- data.frame(Source = row.names(dat.ww), denDF = dat.ww$denDF)

    ifelse(grepl(":", classify),
           pp$Names <- apply(pp[,unlist(strsplit(classify, ":"))], 1, paste, collapse = "_"),
           pp$Names <- pp[[classify]])

    zz <- as.numeric(row.names(pp[!is.na(pp$predicted.value),]))

    SED <- sed[zz,zz]
    Mean <- pp$predicted.value
    Names <-  as.character(pp$Names)
    ndf <- dendf$denDF[grepl(classify, dendf$Source) & nchar(classify) == nchar(dendf$Source)]
    crit.val <- 1/sqrt(2)* qtukey((1-sig), nrow(pp), ndf)*SED
  }

  else {

    pred.out <- predictmeans::predictmeans(model.obj, classify, mplot = FALSE)
    pred.out$mean_table <- pred.out$mean_table[,!grepl("95", names(pred.out$mean_table))]
    sed <- pred.out$`Standard Error of Differences`[1]
    pp <- pred.out$mean_table
    names(pp)[names(pp) == "Predicted means"] <- "predicted.value"
    names(pp)[names(pp) == "Standard error"] <- "std.error"

    SED <- matrix(data = sed, nrow = nrow(pp), ncol = nrow(pp))
    diag(SED) <- NA
    Mean <- pp$predicted.value
    ifelse(grepl(":", classify),
           pp$Names <- apply(pp[,unlist(strsplit(classify, ":"))], 1, paste, collapse = "_"),
           pp$Names <- pp[[classify]])

    Names <-  as.character(pp$Names)
    ndf <- pp$Df[1]
    crit.val <- 1/sqrt(2)* qtukey((1-sig), nrow(pp), ndf)*SED

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
  ordering <- match.arg(order, c('ascending', 'descending', 'increasing', 'decreasing', "default"))

  if(ordering == "ascending" | ordering == "increasing") {
    # Set ordering to FALSE to set decreasing = F in order function
    ordering <- TRUE
  }

  else if(ordering == "descending" | ordering == "decreasing") {
    # Set ordering to TRUE to set decreasing = T in order function
    ordering <- FALSE
  }

  else if(ordering == "default") {
    ordering <- TRUE
  }

  else {
    stop("order must be 'ascending', 'descending' or 'default'")
  }

  ll <- multcompView::multcompLetters3("Names", "predicted.value", diffs, pp, reversed = ordering)

  rr <- data.frame(groups = ll$Letters)
  rr$Names <- row.names(rr)


  pp.tab <- merge(pp,rr)

  # Sorting cases
  # 1. Treatments have an intrinsic order (e.g. numeric year, rate, etc)
  #   a. Increasing
  #   b. Decreasing
  #   c. Default (numeric)
  #   c. User provided
  #   d. None
  # 2. Treatments don't have an intrinsic order
  #   a. Increasing
  #   b. Decreasing
  #   c. Default (alphabetical)
  #   c. User provided
  #   d. None

  if(!is.na(trans)){

    if(is.na(offset)) {
      stop("Please supply an offset value for the transformation using the 'offset' argument. If an offset was not applied, use a value of 0 for the offset argument.")
    }

    if(trans == "sqrt"){
      pp.tab$PredictedValue <- (pp.tab$predicted.value)^2 - ifelse(!is.na(offset), offset, 0)
      pp.tab$ApproxSE <- 2*abs(pp.tab$std.error)*sqrt(pp.tab$PredictedValue)
      if(int.type == "ci"){
        pp.tab$ci <- qt(p = sig, ndf, lower.tail = FALSE) * pp.tab$std.error
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

    if(trans == "log"){
      pp.tab$PredictedValue <- exp(pp.tab$predicted.value) - ifelse(!is.na(offset), offset, 0)
      pp.tab$ApproxSE <- abs(pp.tab$std.error)*pp.tab$PredictedValue
      if(int.type == "ci"){
        pp.tab$ci <- qt(p = sig, ndf, lower.tail = FALSE) * pp.tab$std.error
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
        pp.tab$ci <- qt(p = sig, ndf, lower.tail = FALSE) * pp.tab$std.error
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
        pp.tab$ci <- qt(p = sig, ndf, lower.tail = FALSE) * pp.tab$std.error
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
        pp.tab$ci <- qt(p = sig, ndf, lower.tail = FALSE) * pp.tab$std.error
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
  }

  else {

    if(int.type == "ci"){
      pp.tab$ci <- qt(p = sig, ndf, lower.tail = FALSE) * pp.tab$std.error
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

  pp.tab <- pp.tab[order(pp.tab$predicted.value, decreasing = !ordering),]


  if(class(model.obj)[1] == "asreml"){
    trtindex <- grep("groups", names(pp.tab)) - 3
  }

  else {
    trtindex <- grep("groups", names(pp.tab)) - 4
  }

  trtnam <- names(pp.tab)[1:trtindex]

  i <- 1
  for(i in 1:trtindex){
    pp.tab[[trtnam[i]]] <- forcats::fct_inorder(pp.tab[[trtnam[i]]])
  }

  # rounding to the correct number of decimal places

  pp.tab[[grep("groups", names(pp.tab))-2]] <- round(pp.tab[[grep("groups", names(pp.tab))-2]], decimals)
  pp.tab[[grep("groups", names(pp.tab))-1]] <- round(pp.tab[[grep("groups", names(pp.tab))-1]], decimals)

  if(save) {
    write.csv(pp.tab, file = paste0(savename, ".csv"), row.names = F)
  }

  if(is.na(trans)) {
    plot <- ggplot2::ggplot(data = pp.tab, ggplot2::aes_(x = as.name(classify))) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = low, ymax = up), width = 0.2) +
      ggplot2::geom_text(ggplot2::aes_(x = as.name(classify), y = pp.tab$up, label = pp.tab$groups), vjust = 0, nudge_y = (pp.tab$up-pp.tab$low)*0.5) +
      ggplot2::geom_point(ggplot2::aes(y = predicted.value), color = "black", shape = 16) + ggplot2::theme_bw() +
      ggplot2::labs(x = "", y = "Predicted Value")
  }
  else {
    plot <- ggplot2::ggplot(data = pp.tab, ggplot2::aes_(x = as.name(classify))) +
      ggplot2::geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
      ggplot2::geom_text(ggplot2::aes_(x = as.name(classify), y = pp.tab$up, label = pp.tab$groups), vjust = 0, nudge_y = (pp.tab$up-pp.tab$low)*0.5) +
      ggplot2::geom_point(ggplot2::aes(y = PredictedValue), color = "black", shape = 16) + ggplot2::theme_bw() +
      ggplot2::labs(x = "", y = "Predicted Value")
  }

  return(list(predicted_values = pp.tab, predicted_plot = plot))
}
