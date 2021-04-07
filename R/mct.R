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
#' @param label_height Height of the text labels above the upper error bar on the plot. Default is 0.5 (50%) of the difference between upper and lower error bars above the top error bar.
#' @param rotation Rotate the text output as Treatments within the plot. Allows for easier reading of long treatment labels. Number between 0 and 360 (inclusive) - default 0
#' @param save Logical (default `FALSE`). Save the predicted values to a csv file?
#' @param savename A file name for the predicted values to be saved to. Default is `predicted_values`.
#' @param plottype The type of file to save the plot as. Usually one of `"pdf"`, `"png"`, or `"jpg"`. See [ggplot2::ggsave()] for all possible options.
#' @param pred Deprecated. Use `classify` instead.
#'
#' @importFrom multcompView multcompLetters
#' @importFrom agricolae HSD.test
#' @importFrom predictmeans predictmeans
#' @importFrom stats predict
#' @importFrom ggplot2 ggplot aes_ aes geom_errorbar geom_text geom_point theme_bw labs theme element_text facet_wrap
#'
#' @details Some transformations require that data has a small offset applied, otherwise it will cause errors (for example taking a log of 0, or square root of negative values). In order to correctly reverse this offset, if the `trans` argument is supplied, an offset value must also be supplied. If there was no offset required for a transformation, then use a value of 0 for the `offset` argument.
#'
#' @return A list containing a data frame with predicted means, standard errors, confidence interval upper and lower bounds, and significant group allocations, as well as a plot visually displaying the predicted values.
#'
#' @references Jørgensen, E. & Pedersen, A. R. How to Obtain Those Nasty Standard Errors From Transformed Data - and Why They Should Not Be Used. [http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.47.9023](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.47.9023)
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
                    label_height = 0.5,
                    rotation = 0,
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

    # Check if any treatments are aliased, and remove them and print a warning
    if(anyNA(pred.obj$pvals$predicted.value)) {
      aliased <- which(is.na(pred.obj$pvals$predicted.value))
      # Get the level values of the aliased treatments
      # If only one treatment (classify does not contain :) all levels printed separated with ,
      # If multiple treatments, first need to concatenate columns, then collapse rows
      aliased_names <- pred.obj$pvals[aliased, !names(pred.obj$pvals) %in% c("predicted.value", "std.error", "status")]

      if(grepl(":", classify)) {
        # aliased_names <- as.data.frame(aliased_names)
        aliased_names <- paste(apply(aliased_names, 1, paste, collapse = ":"), collapse = ", ")
      }
      else {
        aliased_names <- paste(aliased_names, collapse = ", ")
      }

      pred.obj$pvals <- pred.obj$pvals[!is.na(pred.obj$pvals$predicted.value),]
      pred.obj$pvals <- droplevels(pred.obj$pvals)
      pred.obj$sed <- pred.obj$sed[-aliased, -aliased]
      warning(paste0("Some levels of ", classify, " are aliased. They have been removed from predicted output.\n  Aliased levels are: ", aliased_names, "\n  These levels are saved in the output object."))
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

    # zz <- as.numeric(1:nrow(pp))

    # SED <- sed[1:nrow(pp),1:nrow(pp)]

    # Mean <- pp$predicted.value
    # Names <-  as.character(pp$Names)
    ndf <- dendf$denDF[grepl(classify, dendf$Source) & nchar(classify) == nchar(dendf$Source)]
    crit.val <- 1/sqrt(2)* qtukey((1-sig), nrow(pp), ndf)*sed

    # Grab the response from the formula to create plot Y label
    ylab <- model.obj$formulae$fixed[[2]]
  }

  else {

    pred.out <- predictmeans(model.obj, classify, mplot = FALSE, ndecimal = decimals)

    pred.out$mean_table <- pred.out$mean_table[,!grepl("95", names(pred.out$mean_table))]
    sed <- pred.out$`Standard Error of Differences`[1]
    pp <- pred.out$mean_table
    names(pp)[names(pp) == "Predicted means"] <- "predicted.value"
    names(pp)[names(pp) == "Standard error"] <- "std.error"

    SED <- matrix(data = sed, nrow = nrow(pp), ncol = nrow(pp))
    diag(SED) <- NA
    # Mean <- pp$predicted.value
    ifelse(grepl(":", classify),
           pp$Names <- apply(pp[,unlist(strsplit(classify, ":"))], 1, paste, collapse = "_"),
           pp$Names <- pp[[classify]])

    # Names <-  as.character(pp$Names)
    ndf <- pp$Df[1]
    crit.val <- 1/sqrt(2)* qtukey((1-sig), nrow(pp), ndf)*SED

    # Grab the response from the formula to create plot Y label
    ylab <- model.obj$terms[[2]]
  }

  # Check that the predicted levels don't contain a dash -, if they do replace and display warning
  if(any(grepl("-", pp[,1]))) {
    levs <- grep("-", pp[,1], value = T)
    if(length(levs)>1) {
      warning("The treatment levels ", paste(levs, collapse = ", "), "contained '-', which has been replaced in the final output with '_'")
    }
    else {
      warning("The treatment level ", levs, " contained '-', which has been replaced in the final output with '_'")
    }
    pp[,1] <- gsub(pattern = "-", replacement = "_", pp[,1])
    pp$Names <- gsub(pattern = "-", replacement = "_", pp$Names)
  }

  Names <-  as.character(pp$Names)

  # Determine pairs that are significantly different
  diffs <- abs(outer(pp$predicted.value, pp$predicted.value, "-")) > crit.val
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
    pp.tab[[trtnam[i]]] <- factor(pp.tab[[trtnam[i]]], levels = unique(pp.tab[[trtnam[i]]]))
  }

  # rounding to the correct number of decimal places
  pp.tab <- rapply(object = pp.tab, f = round, classes = "numeric", how = "replace", digits = decimals)
  # pp.tab[[grep("groups", names(pp.tab))-2]] <- round(pp.tab[[grep("groups", names(pp.tab))-2]], decimals)
  # pp.tab[[grep("groups", names(pp.tab))-1]] <- round(pp.tab[[grep("groups", names(pp.tab))-1]], decimals)

  if(save) {
    write.csv(pp.tab, file = paste0(savename, ".csv"), row.names = F)
  }

  # If there are brackets in the label, grab the text from inside
  if(is.call(ylab)) {
    ylab <- as.character(ylab)[2]
  }

  if(grepl(":", classify)) {
    split_classify <- unlist(strsplit(classify, ":"))
    if(length(split_classify)>2) {
      classify3 <- split_classify[3]
    }
    classify2 <- split_classify[2]
    classify <- split_classify[1]
  }

  if(is.na(trans)) {
    plot <- ggplot2::ggplot(data = pp.tab, ggplot2::aes_(x = as.name(classify))) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = low, ymax = up), width = 0.2) +
      ggplot2::geom_text(ggplot2::aes_(x = as.name(classify), y = pp.tab$up, label = pp.tab$groups), vjust = 0, nudge_y = (pp.tab$up-pp.tab$low)*label_height) +
      ggplot2::geom_point(ggplot2::aes(y = predicted.value), color = "black", shape = 16) + ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = rotation)) +
      ggplot2::labs(x = "", y = paste0("Predicted ", ylab))
  }
  else {
    plot <- ggplot2::ggplot(data = pp.tab, ggplot2::aes_(x = as.name(classify))) +
      ggplot2::geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
      ggplot2::geom_text(ggplot2::aes_(x = as.name(classify), y = pp.tab$up, label = pp.tab$groups), vjust = 0, nudge_y = (pp.tab$up-pp.tab$low)*label_height) +
      ggplot2::geom_point(ggplot2::aes(y = PredictedValue), color = "black", shape = 16) + ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = rotation)) +
      ggplot2::labs(x = "", y = paste0("Predicted ", ylab))
  }

  if(exists("classify3")) {
    plot <- plot + ggplot2::facet_wrap(as.formula(paste("~", classify2, "+", classify3)))
  }
  else if(exists("classify2")) {
    plot <- plot + ggplot2::facet_wrap(as.formula(paste("~", classify2)))
  }

  output <- list(predicted_values = pp.tab, predicted_plot = plot)
  if(exists("aliased_names")) {
    output$aliased <- aliased_names
  }

  return(output)
}
