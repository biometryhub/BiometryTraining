# Comp to asreml
library(asreml)
oats <- asreml::oats
oats <- oats[order(oats$Row, oats$Column),]
model.asr <- asreml(yield ~ Nitrogen + Variety + Nitrogen:Variety,
                    random = ~ Blocks + Blocks:Wplots,
                    residual = ~ ar1(Row):ar1(Column),
                    data = oats)


vario <- function(model.obj) {
    # So the 'z' value for the variogram is the residuals
    # Need to be able to pull out the x/y from the model object

    vals <- model.obj$mf[, c('Row', 'Column')]
    vals <- cbind(vals, resid = resid(model.obj))


    rows <- max(as.numeric(vals$Row))
    cols <- max(as.numeric(vals$Column))

    vario <- expand.grid(Row = 0:(rows-1), Column = 0:(cols-1))

    # Ignore the 0, 0 case (gamma=0, counted row*cols times)
    gammas <- c(0)
    nps <- c(rows * cols)

    for (index in 2:nrow(vario)) {
        i <- vario[index, 'Row']
        j <- vario[index, 'Column']

        gamma <- 0
        np <- 0
        for (val_index in 1:nrow(vals)) {
            val <- vals[val_index, ]

            # Deliberate double-counting so that offset handling is easy
            # (so e.g. we compute distance from (1,1)->(2,3), and then again
            # later from (2,3)->(1,1)).
            for (offset in unique(list(c(i, j), c(-i, j), c(i, -j), c(-i, -j)))) {
                row <- as.numeric(val['Row']) + offset[1]
                col <- as.numeric(val['Column']) + offset[2]

                if (0 < row && row <= rows && 0 < col && col <= cols) {
                    other <- vals[which(vals['Row'] == row & vals['Column'] == col), ]
                    gamma <- gamma + (as.numeric(val['resid'])-as.numeric(other['resid']))^2
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

        gammas <- append(gammas, gamma)
        nps <- append(nps, np)
    }
    vario <- cbind(vario, data.frame(gamma = gammas, np = nps))
}

# Direct comparison
# View(cbind(vario, asreml::varioGram(model.asr)))

# vario(model.asr)
