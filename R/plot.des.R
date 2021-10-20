#' Produces graph of design layout
#'
#' @param design.obj An `agricolae` design object.
#' @param nrows The number of rows in the design.
#' @param ncols The number of columns in the design.
#' @param brows For RCBD only. The number of rows in a block.
#' @param bcols For RCBD only. The number of columns in a block.
#' @param byrow For split-plot only. Logical. Provides a way to arrange plots within whole-plots when there are multiple possible arrangements.
#' @param rotation Rotate the text output as Treatments within the plot. Allows for easier reading of long treatment labels. Number between 0 and 360 (inclusive) - default 0
#' @param size Increase or decrease the text size within the plot for treatment labels. Numeric with default value of 4.
#' @param margin Logical (default FALSE). Expand the plot to the edges of the plotting area i.e. remove whitespace between plot and axes.
#' @param quiet Logical (default FALSE). Return the objects without printing output.
#' @param return.seed Logical (default TRUE). Output the seed used in the design?
#' @param fac.sep Character to separate factorial treatments.
#'
#' @return Returns dataframe of design and ggplot object of design layout.
#'
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggplot2 ggplot geom_tile aes geom_text theme_bw scale_fill_manual scale_x_continuous scale_y_continuous scale_y_reverse
#' @importFrom scales reverse_trans
#' @importFrom stringi stri_sort
#' @keywords internal
#'
plot.des <- function(design.obj, nrows, ncols, brows, bcols, byrow, rotation, size, margin, return.seed, fac.sep) {
    # Asign NULL to variables that give a NOTE in package checks
    # Known issue. See https://www.r-bloggers.com/no-visible-binding-for-global-variable/
    treatments <- NULL
    xmin <- NULL
    xmax <- NULL
    ymin <- NULL
    ymax <- NULL
    Row <- NULL

    if(!missing(fac.sep) && length(fac.sep) == 1) {
        fac.sep <- rep(fac.sep, times = 2)
    }

    if (return.seed) {
        des.seed <- design.obj$parameters$seed
    }
    else {
        des.seed <- NULL
    }

    ifelse(design.obj$parameters$design == "factorial",
           design <- paste("factorial", design.obj$parameters$applied, sep = "_"),
           design <- design.obj$parameters$design
    )

    if (design == "crd") {
        plan <- expand.grid(row = 1:nrows, col = 1:ncols)
        des <- cbind(plan, design.obj$book)

        names(des)[names(des)=="r"] <- "rep"
        names(des)[names(des)=="trt"] <- "treatments"
        ntrt <- nlevels(as.factor(des$treatments))
    }

    if (design == "rcbd") {
        names(design.obj$book)[names(design.obj$book)=="trt"] <- "treatments"
        ntrt <- nlevels(as.factor(design.obj$book$treatments))

        # Calculate direction of blocking
        xx <- c()
        rr <- nrows / brows
        cc <- ncols / bcols
        # Blocking across rows: brows == ntrt in a single column
        if (brows == ntrt) {
            des <- design.obj$book
            plan <- expand.grid(row = 1:nrows, col = 1:ncols) # 2
        }

        # Blocking incomplete rows all columns
        if (rr > 1 & cc == 1) {
            des <- design.obj$book
            plan <- expand.grid(col = 1:ncols, row = 1:nrows) # 1
        }


        # Blocking incomplete rows and incomplete columns
        if (rr > 1 & cc > 1) {
            des <- design.obj$book

            # set up empty columns in the plan data.frame
            plan <- expand.grid(row = 1:nrows, col = 1:ncols)
            plan$block <- des$block
            plan$col <- NA
            plan$row <- NA

            pp <- expand.grid(col = 1:bcols, row = 1:brows)

            i <- 1
            for (j in 1:rr) {
                for (k in 1:cc) {
                    plan$col[plan$block == i] <- pp$col + (k - 1) * bcols
                    plan$row[plan$block == i] <- pp$row + (brows * (j - 1))
                    k <- k + 1
                    i <- i + 1
                }
                j <- j + 1
            }
            plan$block <- NULL
        } # 3


        # Blocking across columns: bcols == ntrt in a single row
        if (bcols == ntrt) {
            des <- design.obj$book
            plan <- expand.grid(col = 1:ncols, row = 1:nrows)
        } # 4


        # Blocking incomplete columns all rows
        if (cc > 1 & rr == 1) {
            des <- design.obj$book

            # set up empty columns in the plan data.frame
            plan <- expand.grid(row = 1:nrows, col = 1:ncols)
            plan$block <- des$block
            plan$col <- NA
            plan$row <- NA

            pp <- expand.grid(col = 1:bcols, row = 1:brows)

            i <- 1
            for (k in 1:cc) {
                plan$col[plan$block == i] <- pp$col + (k - 1) * bcols
                plan$row[plan$block == i] <- pp$row
                k <- k + 1
                i <- i + 1
            }
            plan$block <- NULL
        } # 5

        des <- cbind(plan, des)
    }

    if (design == "lsd") {
        des <- design.obj$book
        des$row <- as.numeric(des$row)
        des$col <- as.numeric(des$col)

        names(des)[4] <- "treatments"
        ntrt <- nlevels(as.factor(des$treatments))
    }


    if (design == "factorial_crd") {
        treatments <- NULL
        plan <- expand.grid(row = 1:nrows, col = 1:ncols)
        des <- cbind(plan, design.obj$book, row.names = NULL)

        for (i in 3:ncol(design.obj$book)) {
            treatments <- paste(treatments, paste(colnames(design.obj$book)[i], design.obj$book[, i], sep = fac.sep[1]), sep = fac.sep[2])
        }

        if(fac.sep[2] == "") {
            des$treatments <- factor(trimws(treatments))
        }
        else {
            des$treatments <- factor(trimws(substr(treatments, 2, nchar(treatments))))
        }
        names(des)[names(des)=="r"] <- "reps"
        ntrt <- nlevels(des$treatments)
    }

    if (design == "factorial_rcbd") {
        treatments <- NULL

        for (i in 3:ncol(design.obj$book)) {
            treatments <- paste(treatments, paste(colnames(design.obj$book)[i], design.obj$book[, i], sep = fac.sep[1]), sep = fac.sep[2])
        }

        if(fac.sep[2] == "") {
            design.obj$book$treatments <- factor(trimws(treatments))
        }
        else {
            design.obj$book$treatments <- factor(trimws(substr(treatments, 2, nchar(treatments))))
        }
        ntrt <- nlevels(as.factor(design.obj$book$treatments))

        # Calculate direction of blocking
        xx <- c()
        rr <- nrows / brows
        cc <- ncols / bcols
        # Blocking across rows: brows == ntrt in a single column
        if (brows == ntrt) {
            des <- design.obj$book
            plan <- expand.grid(row = 1:nrows, col = 1:ncols) # 2
        }

        # Blocking incomplete rows all columns
        if (rr > 1 & cc == 1) {
            des <- design.obj$book
            plan <- expand.grid(col = 1:ncols, row = 1:nrows) # 1
        }


        # Blocking incomplete rows and incomplete columns
        if (rr > 1 & cc > 1) {
            des <- design.obj$book

            # set up empty columns in the plan data.frame
            plan <- expand.grid(row = 1:nrows, col = 1:ncols)
            plan$block <- des$block
            plan$col <- NA
            plan$row <- NA

            pp <- expand.grid(col = 1:bcols, row = 1:brows)

            i <- 1
            for (j in 1:rr) {
                for (k in 1:cc) {
                    plan$col[plan$block == i] <- pp$col + (k - 1) * bcols
                    plan$row[plan$block == i] <- pp$row + (brows * (j - 1))
                    k <- k + 1
                    i <- i + 1
                }
                j <- j + 1
            }
            plan$block <- NULL
        } # 3


        # Blocking across columns: bcols == ntrt in a single row
        if (bcols == ntrt) {
            des <- design.obj$book
            plan <- expand.grid(col = 1:ncols, row = 1:nrows)
        } # 4


        # Blocking incomplete columns all rows
        if (cc > 1 & rr == 1) {
            des <- design.obj$book

            # set up empty columns in the plan data.frame
            plan <- expand.grid(row = 1:nrows, col = 1:ncols)
            plan$block <- des$block
            plan$col <- NA
            plan$row <- NA

            pp <- expand.grid(col = 1:bcols, row = 1:brows)

            i <- 1
            for (k in 1:cc) {
                plan$col[plan$block == i] <- pp$col + (k - 1) * bcols
                plan$row[plan$block == i] <- pp$row
                k <- k + 1
                i <- i + 1
            }
            plan$block <- NULL
        } # 5

        des <- cbind(plan, des)
    }

    if (design == "factorial_lsd") {
        treatments <- NULL
        des <- design.obj$book

        for (i in 4:ncol(design.obj$book)) {
            treatments <- paste(treatments, paste(colnames(design.obj$book)[i], design.obj$book[, i], sep = fac.sep[1]), sep = fac.sep[2])
        }

        if(fac.sep[2] == "") {
            des$treatments <- factor(trimws(treatments))
        }
        else {
            des$treatments <- factor(trimws(substr(treatments, 2, nchar(treatments))))
        }

        ntrt <- nlevels(des$treatments)
        des$row <- as.numeric(des$row)
        des$col <- as.numeric(des$col)
    }

    if (design == "split") {
        des <- design.obj$book
        spfacs <- c("plots", "splots", "block")

        trtNams <- names(des[!is.element(names(des), spfacs)])


        des$treatments <- factor(paste(des[, trtNams[1]], des[, trtNams[2]], sep = "_"))

        # Number of treatments
        ntrt <- nlevels(des$treatments)


        # Calculate direction of blocking
        xx <- c()
        rr <- nrows / brows
        cc <- ncols / bcols
        # Blocking across rows: brows == ntrt in a single column
        if (brows == ntrt) {
            plan <- expand.grid(row = 1:nrows, col = 1:ncols) # 2
        }

        # Blocking incomplete rows all columns
        if (rr > 1 & cc == 1) {
            plan <- expand.grid(col = 1:ncols, row = 1:nrows) # 1
        }

        # Blocking incomplete rows and incomplete columns
        if (rr > 1 & cc > 1) {

            # set up empty columns in the plan data.frame
            plan <- expand.grid(row = 1:nrows, col = 1:ncols)
            plan$block <- des$block
            plan$col <- NA
            plan$row <- NA

            pp <- expand.grid(col = 1:bcols, row = 1:brows)

            i <- 1
            for (j in 1:rr) {
                for (k in 1:cc) {
                    plan$col[plan$block == i] <- pp$col + (k - 1) * bcols
                    plan$row[plan$block == i] <- pp$row + (brows * (j - 1))
                    k <- k + 1
                    i <- i + 1
                }
                j <- j + 1
            }
            plan$block <- NULL
        } # 3


        # Blocking across columns: bcols == ntrt in a single row
        if (bcols == ntrt) {
            plan <- expand.grid(col = 1:ncols, row = 1:nrows)
        } # 4


        # Blocking incomplete columns all rows
        if (cc > 1 & rr == 1) {

            # set up empty columns in the plan data.frame
            plan <- expand.grid(row = 1:nrows, col = 1:ncols)
            plan$block <- des$block
            plan$col <- NA
            plan$row <- NA

            pp <- expand.grid(col = 1:bcols, row = 1:brows)

            i <- 1
            for (k in 1:cc) {
                plan$col[plan$block == i] <- pp$col + (k - 1) * bcols
                plan$row[plan$block == i] <- pp$row
                k <- k + 1
                i <- i + 1
            }
            plan$block <- NULL
        } # 5

        des <- cbind(plan, des)
        # Order by column within blocks, rather than row default
        if(!byrow) {
            des[,c("row", "col", "block")] <- des[order(des$block, des$col, des$row), c("row", "col", "block")]
        }
    }

    des$treatments <- factor(des$treatments, levels = unique(stringi::stri_sort(des$treatments, numeric = TRUE)))

    # des <- dplyr::mutate(des, row = factor(row),
    # row = factor(row, levels = rev(levels(row))))

    # create the colours for the graph
    color_palette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))(ntrt)

    if (!any(grepl("block", names(des)))) {

        # create the graph
        plt <- ggplot2::ggplot() +
            ggplot2::geom_tile(data = des, mapping = ggplot2::aes(x = col, y = row, fill = treatments), colour = "black") +
            ggplot2::geom_text(data = des, mapping = ggplot2::aes(x = col, y = row, label = treatments), angle = rotation, size = size) +
            ggplot2::theme_bw() +
            ggplot2::scale_fill_manual(values = color_palette, name = "Treatment")
    }
    if (any(grepl("block", names(des)))) {

        # Set up dataframe with coordinates for drawing the blocks
        blkdf <- data.frame(
            block = sort(unique(des$block)),
            xmin = 0, xmax = 0, ymin = 0, ymax = 0
        )
        for (i in 1:nrow(blkdf)) {
            item <- blkdf$block[i]
            tmp <- des[des$block == item, ]
            blkdf[i, "ymin"] <- (min(tmp$row) - 0.5)
            blkdf[i, "ymax"] <- (max(tmp$row) + 0.5)
            blkdf[i, "xmin"] <- (min(tmp$col) - 0.5)
            blkdf[i, "xmax"] <- (max(tmp$col) + 0.5)
        }

        plt <- ggplot2::ggplot() +
            ggplot2::geom_tile(data = des, mapping = ggplot2::aes(x = col, y = row, fill = treatments), colour = "black") +
            ggplot2::geom_text(data = des, mapping = ggplot2::aes(x = col, y = row, label = treatments), angle = rotation, size = size) +
            #        xlim(0,max(des$col)+1) + ylim(0,max(des$row)+1) +
            ggplot2::geom_rect(
                data = blkdf,
                mapping = ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                size = 1.8, colour = "black", fill = NA
            ) +
            ggplot2::geom_rect(
                data = blkdf,
                mapping = ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                size = 0.6, colour = "white", fill = NA
            ) +
            ggplot2::theme_bw() +
            ggplot2::scale_fill_manual(values = color_palette, name = "Treatment") #+ ggplot2::scale_y_continuous(trans = scales::reverse_trans())
    }

    if (!margin) {
        plt <- plt + ggplot2::scale_x_continuous(expand = c(0, 0), breaks = seq(1, max(des$col), 1)) + ggplot2::scale_y_continuous(expand = c(0, 0), trans = scales::reverse_trans(), breaks = seq(1, max(des$row), 1))
    }
    else {
        plt <- plt + ggplot2::scale_y_continuous(trans = scales::reverse_trans(), breaks = seq(1, max(des$row), 1)) + ggplot2::scale_x_continuous(breaks = seq(1, max(des$col), 1))
    }

    return(list(design = des, seed = des.seed, plot.des = plt))
}
