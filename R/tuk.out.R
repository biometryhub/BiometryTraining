tuk.out <- function(pred.obj, model.obj, data, pred, sig = 0.95){

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
    pp$est.status <- NULL

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

    return(pp)
}
