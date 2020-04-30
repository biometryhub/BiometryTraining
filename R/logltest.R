#' Log-likelihood test for comparing terms in ASreml-R models
#'
#' @param model.obj An ASreml-R model object
#' @param rand.terms Random terms from the model
#' @param resid.terms Residual terms from the model
#'
#' @importFrom lucid vc
#' @importFrom asremlPlus REMLRT
#'
#' @return A dataframe containing the results of the test.
#' @export
#'
#' @examples
logl.test <- function(model.obj, rand.terms, resid.terms){

    #load required packages
    # if(!require(lucid)){
    #     install.packages("lucid")
    # }
    #     library(lucid)

    # if(!require(asremlPlus)){
    #     install.packages("asremlPlus")
    # }
    #     library(asremlPlus)

    # Find terms on the boundary

    bnd <- names(lucid::vc(model.obj)$bound[lucid::vc(model.obj)$bound == "B"])

    if(any(grepl("!cor", bnd))){
        trm <- substring(bnd[grepl("!cor", bnd, fixed = TRUE)],
                         (unlist(gregexpr("!", bnd[grepl("!cor", bnd, fixed = TRUE)]))[1]+1),
                         (unlist(gregexpr("!", bnd[grepl("!cor", bnd, fixed = TRUE)]))[2]-1))

        bnd[grepl("!cor", bnd)] <- resid.terms[grepl(trm, resid.terms)]
    }

    all.terms <- c(rand.terms, resid.terms)
    all.bnd <- all(is.element(all.terms, bnd))

    # terms to conduct loglikehood ratio test on
    tt <- c(rand.terms[!is.element(rand.terms, bnd)], resid.terms[!is.element(resid.terms, bnd)])

    if(length(bnd) >0){
        test.df <- data.frame(Term = bnd, LogLRT.pvalue = 1)
    } else {
        test.df <- data.frame(Term = character(), LogLRT.pvalue = numeric())
    }

    # Loglikehood ratio tests
    if(!all.bnd){

        # update model excluding the boundary terms - random
        brand.terms <- c()
        brand.terms <- rand.terms[is.element(rand.terms, bnd)]

        if(length(brand.terms > 0)){
            model.obj <- update(model.obj, random = as.formula(paste("~ . - ", paste(brand.terms, collapse = " - "), sep = " ")))
            n <- 1
            while(!model.obj$converge & n < 6){
                model.obj <- update(model.obj)
                n <- n + 1
            }

            while(any(model.obj$vparameters.pc > 1)){
                model.obj <- update(model.obj)
            }
        }

        # Fitting the models

        for(i in 1:length(tt)){

            if(grepl("ar", tt[i])){
                tt.new <- paste("id", substring(tt[i],4), sep = "")
                old.resid <- substring(toString(dat.asr$formulae$residual), 4)
                new.resid <- gsub(tt[[i]], tt.new, old.resid, fixed = TRUE)

                # Fit reduced model
                dat.asr1 <- update(model.obj, residual = as.formula(paste("~", new.resid,sep = " ")))

                n <- 1
                while(!dat.asr1$converge & n < 6){
                    dat.asr1 <- update(dat.asr1)
                    n <- n + 1
                }

                while(any(dat.asr1$vparameters.pc > 1)){
                    dat.asr1 <- update(dat.asr1)
                }

                # Logl test
                ll.test <- asremlPlus::REMLRT(h1.asreml.obj = model.obj, h0.asreml.obj = dat.asr1)$p

                result.df <- data.frame(Term = tt[i], LogLRT.pvalue = ll.test)
                test.df <- rbind(test.df, result.df)

            }
            if(!grepl("ar", tt[i])){

                # Fit reduced model
                tst.terms <- tt[grepl(tt[i], tt)]
                dat.asr1 <- update(model.obj, random = as.formula(paste("~ . - ", paste(tst.terms, collapse = " - "), sep = " ")))

                n <- 1
                while(!dat.asr1$converge & n < 6){
                    dat.asr1 <- update(dat.asr1)
                    n <- n + 1
                }

                while(any(dat.asr1$vparameters.pc > 1)){
                    dat.asr1 <- update(dat.asr1)
                }

                # Logl test
                ll.test <- asremlPlus::REMLRT(h1.asreml.obj = model.obj, h0.asreml.obj = dat.asr1)$p

                result.df <- data.frame(Term = tt[i], LogLRT.pvalue = ll.test)
                test.df <- rbind(test.df, result.df)

            }
        }
    }

    return(test.df)

}
