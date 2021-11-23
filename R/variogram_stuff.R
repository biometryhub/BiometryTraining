# Comp to asreml
# library(asreml)
# oats <- asreml::oats
# oats <- oats[order(oats$Row, oats$Column),]
# model.asr <- asreml(yield ~ Nitrogen + Variety + Nitrogen:Variety,
#                     random = ~ Blocks + Blocks:Wplots,
#                     residual = ~ ar1(Row):ar1(Column),
#                     data = oats)


vario <- function(model.obj) {
    # So the 'z' value for the variogram is the residuals
    # Need to be able to pull out the x/y from the model object

    dims <- unlist(strsplit(names(model.obj$R.param[1]), ":"))
    vals <- cbind(data.frame(model.obj$mf[[dims[1]]]), data.frame(model.obj$mf[[dims[2]]]), resid = resid(model.obj))
    colnames(vals) <- c(dims, "resid")
    Row <- as.numeric(model.obj$mf[[dims[1]]])
    Column <- as.numeric(model.obj$mf[[dims[2]]])
    Resid <- residuals(model.obj)

    nrows <- max(Row)
    ncols <- max(Column)

    vario <- expand.grid(Row = 0:(nrows-1), Column = 0:(ncols-1))

    # Ignore the 0, 0 case (gamma=0, counted row*cols times)
    gammas <- rep(0, nrows*ncols)
    nps <- rep(nrows*ncols, nrows*ncols)

    for (index in 2:nrow(vario)) {
        i <- vario[index, 'Row']
        j <- vario[index, 'Column']

        gamma <- 0
        np <- 0
        for (val_index in 1:nrow(vals)) {
            # val <- vals[val_index, ]

            # Deliberate double-counting so that offset handling is easy
            # (so e.g. we compute distance from (1,1)->(2,3), and then again
            # later from (2,3)->(1,1)).
            for (offset in unique(list(c(i, j), c(-i, j), c(i, -j), c(-i, -j)))) {
                row <- Row[val_index] + offset[1]
                col <- Column[val_index] + offset[2]

                if (0 < row && row <= nrows && 0 < col && col <= ncols) {
                    other <- which(Row == row & Column == col)
                    gamma <- gamma + (Resid[val_index]-Resid[other])^2
                    np <- np + 1
                }
            }
        }
        # Since we double-counted precisely, halve to get the correct answer.
        np <- np / 2
        gamma <- gamma / 2

        if (np > 0) {
            gamma <- gamma / (2*np)
        }

        gammas[index] <- gamma
        nps[index] <- np
    }
    vario <- cbind(vario, data.frame(gamma = gammas, np = nps))
    colnames(vario) <- c(dims, "gamma", "np")
    class(vario) <- c("varioGram", "data.frame")
    return(vario)
}

# Direct comparison
# View(cbind(vario, asreml::varioGram(model.asr)))

# v1 <- vario(model.asr)
# v2 <- asreml::varioGram(model.asr)
# cbind(v1, v2)
#
# library(dplyr)
# new_data <- design(type = "split", 1:5, sub_treatments = 1:20, 4, 20, 20, brows = 5, bcols = 20, seed = 42, quiet = T)
# new_data <- new_data$design
#
# new_data <- new_data[order(new_data$row, new_data$col),]
# new_data$yield <- rnorm(400)
#
# new_data <- new_data %>% mutate(across(1:7, factor))
#
# model.asr <- asreml(yield ~ treatments * sub_treatments,
#                     random = ~ block + block:plots,
#                     residual = ~ ar1(row):ar1(col),
#                     data = new_data)
#
# v1 <- vario(model.asr)
# v2 <- asreml::varioGram(model.asr)

