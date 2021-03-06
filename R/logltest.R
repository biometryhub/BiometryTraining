#' Log-likelihood test for comparing terms in ASreml-R models
#'
#' @param model.obj An ASreml-R model object
#' @param rand.terms Random terms from the model. Default is NULL.
#' @param resid.terms Residual terms from the model. Default is NULL.
#'
#' @importFrom lucid vc
#' @importFrom stats as.formula update pchisq
#'
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
logl.test <- function(model.obj, rand.terms = NULL, resid.terms = NULL) {

  # dat.asr <- NULL

  # Find terms on the boundary

  # Supress ASreml output
  sink(tempfile())
  on.exit(sink())
  n <- rep(0, 6)
  warns <- character()

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
      model.obj <- suppressWarnings(update(model.obj, random = as.formula(paste("~ . - ", paste(brand.terms, collapse = " - "), sep = " "))))
      n[1] <- 1
      while (!model.obj$converge & n[1] < 10) {
        model.obj <- suppressWarnings(update(model.obj))
        n[1] <- n[1] + 1
        if(n[1]==10) {
          # warns <- c(warns, 'Model did not converge')
        }
      }

      n[2] <- 1
      while (any(model.obj$vparameters.pc > 1) & n[2] < 10) {
        model.obj <- suppressWarnings(update(model.obj))
        n[2] <- n[2] + 1
        if(n[2]==10) {
          # warns <- c(warns, 'Model did not converge')
        }
      }
    }

    # Fitting the models

    for (i in 1:length(tt)) {
      if (grepl("ar", tt[i])) {
        tt.new <- paste("id", substring(tt[i], 4), sep = "")
        old.resid <- substring(toString(model.obj$formulae$residual), 4)
        new.resid <- gsub(tt[[i]], tt.new, old.resid, fixed = TRUE)

        # Fit reduced model
        model.obj1 <- suppressWarnings(update(model.obj, residual = as.formula(paste("~", new.resid, sep = " "))))

        n[3] <- 1
        while (!model.obj1$converge & n[3] < 10) {
          model.obj1 <- suppressWarnings(update(model.obj1))
          n[3] <- n[3] + 1
          if(n[3]==10) {
            warns <- c(warns, 'Model did not converge')
          }
        }

        n[4] <- 1
        while (any(model.obj1$vparameters.pc > 1) & n[4] < 10) {
          model.obj1 <- suppressWarnings(update(model.obj1))
          n[4] <- n[4] + 1
          if(n[4]==10) {
            warns <- c(warns, 'Model did not converge')
          }
        }

        # Logl test

        p <- (length(model.obj$vparameters) +
                length(model.obj$coefficients$fixed))-
          (length(model.obj1$vparameters) +
             length(model.obj1$coefficients$fixed))

        logl <- 2*(model.obj$loglik-model.obj1$loglik)

        ll.test <- round(1-pchisq(logl, p),3)

        result.df <- data.frame(Term = tt[i], LogLRT.pvalue = ll.test)
        test.df <- rbind(test.df, result.df)
      }
      if (!grepl("ar", tt[i])) {

        # Fit reduced model
        tst.terms <- tt[grepl(tt[i], tt)]
        model.obj1 <- update(model.obj, random = as.formula(paste("~ . - ", paste(tst.terms, collapse = " - "), sep = " ")))

        n5 <- 1
        while (!model.obj1$converge & n5 < 10) {
          model.obj1 <- update(model.obj1)
          n5 <- n5 + 1
          if(n[5]==10) {
            warns <- c(warns, 'Model did not converge')
          }
        }

        n6 <- 1
        while (any(model.obj1$vparameters.pc > 1) & n6 < 10) {
          model.obj1 <- update(model.obj1)
          n6 <- n6 + 1
          if(n[6]==10) {
            warns <- c(warns, 'Model did not converge')
          }
        }

        # Logl test
        p <- (length(model.obj$vparameters) +
                length(model.obj$coefficients$fixed))-
          (length(model.obj1$vparameters) +
             length(model.obj1$coefficients$fixed))
        logl <- -2*(model.obj1$loglik-model.obj$loglik)

        ll.test <- round(1-pchisq(abs(logl), p),3)

        result.df <- data.frame(Term = tt[i], LogLRT.pvalue = ll.test)

        test.df <- rbind(test.df, result.df)
      }
    }
  }

  test.df$LogLRT.pvalue <- round(test.df$LogLRT.pvalue, 3)

  all.terms <- c(rand.terms, resid.terms)
  test.df <- test.df[is.element(test.df$Term, all.terms),]

  if(length(warns) > 0) {
    warning(warns[1], call. = F)
  }

  return(test.df)
}
