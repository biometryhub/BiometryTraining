#' Log-likelihood test for comparing terms in ASreml-R models
#'
#' @param model.obj An ASreml-R model object
#' @param rand.terms Random terms from the model
#' @param resid.terms Residual terms from the model
#'
#' @importFrom lucid vc
#' @importFrom asremlPlus REMLRT
#' @importFrom stats as.formula update
#'
#' @details Uses the [asremlPlus::REMLRT.asreml()] function to calculate the Likelihood ratio test for Asreml-R objects.
#'
#' @return A dataframe containing the results of the test.
#' @export
#'
#' @examples
#' \dontrun{
#' library(agridat)
#' dat <- yates.oats
#' dat$row <- factor(dat$row)
#' dat$col <- factor(dat$col)
#' dat$nitro <- factor(dat$nitro)
#' dat$mplot <- factor(as.numeric(dat$gen))
#' dat <- dat[order(dat$row, dat$col), ]
#' library(asreml)
#' dat.asr <- asreml(yield ~ gen + nitro + gen:nitro,
#'   random = ~ block + block:mplot,
#'   residual = ~ ar1(row):id(col), data = dat
#' )
#' oats.logl <- logl.test(
#'   model.obj = dat.asr, rand.terms = c("block", "block:mplot"),
#'   resid.terms = c("ar1(row)")
#' )
#' oats.logl
#' }
#'
logl.test <- function(model.obj, rand.terms, resid.terms) {

  # dat.asr <- NULL

  # Find terms on the boundary

  # Supress ASreml output
  sink(tempfile())
  on.exit(sink())

  bnd <- lucid::vc(model.obj)$effect[(lucid::vc(model.obj)$bound)== "B"]

  if (any(grepl("!cor", bnd))) {
    trm <- substring(
      bnd[grepl("!cor", bnd, fixed = TRUE)],
      (unlist(gregexpr("!", bnd[grepl("!cor", bnd, fixed = TRUE)]))[1] + 1),
      (unlist(gregexpr("!", bnd[grepl("!cor", bnd, fixed = TRUE)]))[2] - 1)
    )

    bnd[grepl("!cor", bnd)] <- resid.terms[grepl(trm, resid.terms)]
  }

  all.terms <- c(rand.terms, resid.terms)
  all.bnd <- all(is.element(all.terms, bnd))

  # terms to conduct loglikehood ratio test on
  tt <- c(rand.terms[!is.element(rand.terms, bnd)], resid.terms[!is.element(resid.terms, bnd)])

  if (length(bnd) > 0) {
    test.df <- data.frame(Term = bnd, LogLRT.pvalue = 1)
  } else {
    test.df <- data.frame(Term = character(), LogLRT.pvalue = numeric())
  }

  # Loglikehood ratio tests
  if (!all.bnd) {

    # update model excluding the boundary terms - random
    brand.terms <- c()
    brand.terms <- rand.terms[is.element(rand.terms, bnd)]

    if (length(brand.terms > 0)) {
      model.obj <- update(model.obj, random = as.formula(paste("~ . - ", paste(brand.terms, collapse = " - "), sep = " ")))
      n <- 1
      while (!model.obj$converge & n < 6) {
        model.obj <- update(model.obj)
        n <- n + 1
      }

      while (any(model.obj$vparameters.pc > 1)) {
        model.obj <- update(model.obj)
      }
    }

    # Fitting the models

    for (i in 1:length(tt)) {
      if (grepl("ar", tt[i])) {
        tt.new <- paste("id", substring(tt[i], 4), sep = "")
        old.resid <- substring(toString(model.obj$formulae$residual), 4)
        new.resid <- gsub(tt[[i]], tt.new, old.resid, fixed = TRUE)

        # Fit reduced model
        model.obj1 <- update(model.obj, residual = as.formula(paste("~", new.resid, sep = " ")))

        n <- 1
        while (!model.obj1$converge & n < 6) {
          model.obj1 <- update(model.obj1)
          n <- n + 1
        }

        while (any(model.obj1$vparameters.pc > 1)) {
          model.obj1 <- update(model.obj1)
        }

        # Logl test
        ll.test <- asremlPlus::REMLRT(h1.asreml.obj = model.obj, h0.asreml.obj = model.obj1)$p

        result.df <- data.frame(Term = tt[i], LogLRT.pvalue = ll.test)
        test.df <- rbind(test.df, result.df)
      }
      if (!grepl("ar", tt[i])) {

        # Fit reduced model
        tst.terms <- tt[grepl(tt[i], tt)]
        model.obj1 <- update(model.obj, random = as.formula(paste("~ . - ", paste(tst.terms, collapse = " - "), sep = " ")))

        n <- 1
        while (!model.obj1$converge & n < 6) {
          model.obj1 <- update(model.obj1)
          n <- n + 1
        }

        while (any(model.obj1$vparameters.pc > 1)) {
          model.obj1 <- update(model.obj1)
        }

        # Logl test
        ll.test <- asremlPlus::REMLRT(h1.asreml.obj = model.obj, h0.asreml.obj = model.obj1)$p

        result.df <- data.frame(Term = tt[i], LogLRT.pvalue = round(ll.test,3))

        test.df <- rbind(test.df, result.df)
      }
    }
  }

  return(test.df)
}
