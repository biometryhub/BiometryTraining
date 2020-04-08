#' Produces graph of design layout
#'
#' @param design.obj An \code{agricolae} design object.
#' @param nrows The number of rows in the design.
#' @param ncols The number of columns in the design.
#' @param brows For RCBD only. The number of rows in a block.
#' @param bcols For RCBD only. The number of columns in a block.
#' @param rotation Rotate the text output as Treatments within the plot. Allows for easier reading of long treatment labels. Number between 0 and 360 (inclusive) - default 0
#' @param size Increase or decrease the text size within the plot for treatment labels. Numeric with default value of 4.
#' @param margin Logical (default FALSE). Expand the plot to the edges of the plotting area i.e. remove whitespace between plot and axes.
#' @param quiet Logical (default FALSE). Return the objects without printing output.
#' @param return.seed Logical (default TRUE). Output the seed used in the design?
#'
#' @return Returns dataframe of design and ggplot object of design layout.
#'
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggplot2 ggplot geom_tile aes geom_text theme_bw scale_fill_manual scale_x_continuous scale_y_continuous scale_y_reverse
#' @importFrom scales reverse_trans
#' @keywords internal
#'
plot.des <- function(design.obj, nrows, ncols, brows, bcols, rotation, size, margin, quiet, return.seed){
    # Asign NULL to variables that give a NOTE in package checks
    # Known issue. See https://www.r-bloggers.com/no-visible-binding-for-global-variable/
    trt <- NULL
    xmin <- NULL
    xmax <- NULL
    ymin <- NULL
    ymax <- NULL
    Row <- NULL

    nth_element <- function(vector, starting_position, n) {
        vector[seq(starting_position, length(vector), n)]
    }

    desfac <- design.obj$parameters$design
    if(return.seed) {
        des.seed <- design.obj$parameters$seed
    }
    else {
        des.seed <- NULL
    }

    ifelse(desfac == "factorial",
           design <- paste("factorial", design.obj$parameters$applied, sep = "_"),
           design <- desfac)

    if(design == "crd"){
        plan <- expand.grid(row = 1:nrows, col = 1:ncols)
        des <- cbind(plan, design.obj$book)

        names(des)[5] <- "trt"
        ntrt <- nlevels(des$trt)
    }

    if(design == "rcbd"){

        names(design.obj$book)[3] <- "trt"
        ntrt <- nlevels(design.obj$book$trt)

        # Calculate direction of blocking
        xx <- c()
        rr <- nrows/brows
        cc <- ncols/bcols
        # Blocking across rows: brows == ntrt in a single column
        if(brows == ntrt){
            des <- design.obj$book
            plan <- expand.grid(row = 1:nrows, col = 1:ncols) #2
        }

        # Blocking incomplete rows all columns
        if(rr > 1 & cc == 1){
            des <- design.obj$book
            plan <- expand.grid(col = 1:ncols, row = 1:nrows) #1
        }


        # Blocking incomplete rows and incomplete columns
        if(rr > 1 & cc > 1){
            des <- design.obj$book

            # set up empty columns in the plan data.frame
            plan <- expand.grid(row = 1:nrows, col = 1:ncols)
            plan$block <- des$block
            plan$col <- NA
            plan$row <- NA

            pp <- expand.grid(col = 1:bcols, row = 1:brows)

            i <- 1
            for(j in 1:rr){
                for(k in 1:cc){
                    plan$col[plan$block == i] <- pp$col + (k-1)*bcols
                    plan$row[plan$block == i] <- pp$row + (brows*(j-1))
                    k <- k + 1
                    i <- i + 1
                }
                j <- j + 1
            }
            plan$block <- NULL
        } #3


        # Blocking across columns: bcols == ntrt in a single row
        if(bcols == ntrt){
            des <- design.obj$book
            plan <- expand.grid(col = 1:ncols, row = 1:nrows)
        } #4


        # Blocking incomplete columns all rows
        if(cc > 1 & rr == 1){
            des <- design.obj$book

            # set up empty columns in the plan data.frame
            plan <- expand.grid(row = 1:nrows, col = 1:ncols)
            plan$block <- des$block
            plan$col <- NA
            plan$row <- NA

            pp <- expand.grid(col = 1:bcols, row = 1:brows)

            i <- 1
            for(k in 1:cc){

                plan$col[plan$block == i] <- pp$col + (k-1)*bcols
                plan$row[plan$block == i] <- pp$row
                k <- k + 1
                i <- i + 1
            }
            plan$block <- NULL
        } #5

        des <- cbind(plan, des)
    }

    if(design == "lsd"){
        des <- design.obj$book
        des$row <- as.numeric(des$row)
        des$col <- as.numeric(des$col)

        names(des)[4] <- "trt"
        ntrt <- nlevels(des$trt)
    }


    if(design == "factorial_crd"){
        plan <- expand.grid(row = 1:nrows, col = 1:ncols)
        des <- cbind(plan, design.obj$book)

        des$trt <- factor(paste("A", des$A, "B", des$B, sep = ""))
        ntrt <- nlevels(des$trt)
    }


    if(design == "factorial_rcbd"){

        design.obj$book$trt <- factor(paste("A", design.obj$book$A, "B", design.obj$book$B, sep = ""))
        ntrt <- nlevels(design.obj$book$trt)

        # Calculate direction of blocking
        xx <- c()
        rr <- nrows/brows
        cc <- ncols/bcols
        # Blocking across rows: brows == ntrt in a single column
        if(brows == ntrt){
            des <- design.obj$book
            plan <- expand.grid(row = 1:nrows, col = 1:ncols) #2
        }

        # Blocking incomplete rows all columns
        if(rr > 1 & cc == 1){
            des <- design.obj$book
            plan <- expand.grid(col = 1:ncols, row = 1:nrows) #1
        }


        # Blocking incomplete rows and incomplete columns
        if(rr > 1 & cc > 1){
            des <- design.obj$book

            # set up empty columns in the plan data.frame
            plan <- expand.grid(row = 1:nrows, col = 1:ncols)
            plan$block <- des$block
            plan$col <- NA
            plan$row <- NA

            pp <- expand.grid(col = 1:bcols, row = 1:brows)

            i <- 1
            for(j in 1:rr){
                for(k in 1:cc){
                    plan$col[plan$block == i] <- pp$col + (k-1)*bcols
                    plan$row[plan$block == i] <- pp$row + (brows*(j-1))
                    k <- k + 1
                    i <- i + 1
                }
                j <- j + 1
            }
            plan$block <- NULL
        } #3


        # Blocking across columns: bcols == ntrt in a single row
        if(bcols == ntrt){
            des <- design.obj$book
            plan <- expand.grid(col = 1:ncols, row = 1:nrows)
        } #4


        # Blocking incomplete columns all rows
        if(cc > 1 & rr == 1){
            des <- design.obj$book

            # set up empty columns in the plan data.frame
            plan <- expand.grid(row = 1:nrows, col = 1:ncols)
            plan$block <- des$block
            plan$col <- NA
            plan$row <- NA

            pp <- expand.grid(col = 1:bcols, row = 1:brows)

            i <- 1
            for(k in 1:cc){

                plan$col[plan$block == i] <- pp$col + (k-1)*bcols
                plan$row[plan$block == i] <- pp$row
                k <- k + 1
                i <- i + 1
            }
            plan$block <- NULL
        } #5

        des <- cbind(plan, des)
    }

    if(design == "factorial_lsd"){
        des <- design.obj$book
        des$trt <- factor(paste("A", des$A, "_B", des$B, sep = ""))
        ntrt <- nlevels(des$trt)
        des$row <- as.numeric(des$row)
        des$col <- as.numeric(des$col)

    }

    if(design == "split"){
        des <- design.obj$book
        plan <- expand.grid(row = 1:nrows, col = 1:ncols)
        des <- cbind(plan, design.obj$book)

        des$trt <- factor(paste(des[,6], des[,7], sep = "_"))

        # Number of treatments
        ntrt <- nlevels(des$trt)
    }

    # des <- dplyr::mutate(des, row = factor(row),
                         # row = factor(row, levels = rev(levels(row))))

    # create the colours for the graph
    color_palette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))(ntrt)

    if(!any(grepl("block", names(des)))){

        # create the graph
        plt <- ggplot2::ggplot() + ggplot2::geom_tile(data = des, mapping = ggplot2::aes(x = col, y = row, fill = trt), colour = "black") +
            ggplot2::geom_text(data = des, mapping = ggplot2::aes(x = col, y = row, label = trt), angle = rotation, size = size) +
            ggplot2::theme_bw() + ggplot2::scale_fill_manual(values = color_palette, name = "Treatment")
    }
    if(any(grepl("block", names(des)))){

        # Set up dataframe with coordinates for drawing the blocks
        blkdf = data.frame(block=sort(unique(des$block)),
                           xmin=0, xmax=0, ymin=0, ymax=0)
        for(i in 1:nrow(blkdf)) {
            item <- blkdf$block[i]
            tmp <- des[des$block == item, ]
            blkdf[i, "ymin"] = (min(tmp$row) - 0.5)
            blkdf[i, "ymax"] = (max(tmp$row) + 0.5)
            blkdf[i, "xmin"] = (min(tmp$col) - 0.5)
            blkdf[i, "xmax"] = (max(tmp$col) + 0.5)
        }

        plt <- ggplot2::ggplot() + ggplot2::geom_tile(data = des, mapping = ggplot2::aes(x = col, y = row, fill = trt), colour = "black") +
            ggplot2::geom_text(data = des, mapping = ggplot2::aes(x = col, y = row, label = trt), angle = rotation, size = size) +
            #        xlim(0,max(des$col)+1) + ylim(0,max(des$row)+1) +
            ggplot2::geom_rect(data = blkdf,
                               mapping = ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                               size = 1.8, colour = "black", fill = NA) + ggplot2::geom_rect(data = blkdf,
                                                                                             mapping = ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                                                                                             size = 0.6, colour = "white", fill = NA) +
            ggplot2::theme_bw() + ggplot2::scale_fill_manual(values = color_palette, name = "Treatment") #+ ggplot2::scale_y_continuous(trans = scales::reverse_trans())
    }

    if(!margin) {
        plt <- plt + ggplot2::scale_x_continuous(expand = c(0,0), breaks=seq(1,max(des$col),1)) + ggplot2::scale_y_continuous(expand = c(0,0), trans = scales::reverse_trans(), breaks=seq(1,max(des$row),1))
    }
    else {
        plt <- plt + ggplot2::scale_y_continuous(trans = scales::reverse_trans(), breaks=seq(1,max(des$row),1)) + ggplot2::scale_x_continuous(breaks=seq(1,max(des$col),1))
    }

    if(!quiet) {
        print(plt)
    }

    return(list(design = des, seed = des.seed, plot.des = plt))
}
