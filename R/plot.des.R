#' Produces graph of design layout
#'
#' @param design.obj An \code{agricolae} design object.
#' @param nrows The number of rows in the design.
#' @param ncols The number of columns in the design.
#' @param brows For RCBD only. The number of rows in a block.
#' @param bcols For RCBD only. The number of columns in a block.
#'
#' @return Returns ggplot object of design layout.
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#'
#' @importFrom ggplot2 ggplot geom_tile aes geom_text theme_bw scale_fill_manual
#' @keywords internal
#'
plot.des <- function(design.obj, nrows, ncols, brows, bcols){

    nth_element <- function(vector, starting_position, n) {
        vector[seq(starting_position, length(vector), n)]
    }

    des <- design.obj$parameters$design

    ifelse(des == "factorial",
           design <- paste("factorial", design.obj$parameters$applied, sep = "_"),
           design <- des)

    if(design == "crd"){
        plan <- expand.grid(row = 1:nrows, col = 1:ncols)
        des <- cbind(plan, design.obj$book)

        names(des)[5] <- "trt"
        ntrt <- nlevels(des$trt)
    }

    if(design == "rcbd"){

        names(design.obj$book)[3] <- "trt"
        ntrt <- nlevels(design.obj$book$trt)

        xx <- c()
        rr <- nrows/brows
        cc <- ncols/bcols
        if(cc < ncols){
            aa <- dim(design.obj$book)[1]/cc
            for(zz in 1:cc){
                for(i in 1:ntrt){
                    vec1 <-((zz-1)*aa+1)
                    vec2 <- zz*aa
                    bb <- nth_element((vec1:vec2), i, ntrt)
                    xx <- c(xx, bb)
                }}
            des <- design.obj$book[xx,]
        } else {
            des <- design.obj$book
        }


        plan <- expand.grid(row = 1:nrows, col = 1:ncols)
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


        xx <- c()
        rr <- nrows/brows
        cc <- ncols/bcols
        if(cc < ncols){
            aa <- dim(design.obj$book)[1]/cc
            for(zz in 1:cc){
                for(i in 1:ntrt){
                    vec1 <-((zz-1)*aa+1)
                    vec2 <- zz*aa
                    bb <- nth_element((vec1:vec2), i, ntrt)
                    xx <- c(xx, bb)
                }}
            des <- design.obj$book[xx,]
        } else {
            des <- design.obj$book
        }
        plan <- expand.grid(row = 1:nrows, col = 1:ncols)
        des <- cbind(plan, des)
    }




    if(design == "factorial_lsd"){
        des$trt <- factor(paste("A", des$A, "B", des$B, sep = ""))
        ntrt <- nlevels(des$trt)

        des <- design.obj$book
        des$row <- as.numeric(des$row)
        des$col <- as.numeric(des$col)

    }





    if(design == "split"){

        plan <- expand.grid(row = 1:nrows, col = 1:ncols)
        des <- cbind(plan, design.obj$book)

        des$trt <- factor(paste("wp", des[,6], "sp", des[,7], sep = ""))

        # Number of treatments
        ntrt <- nlevels(des$trt)
    }



    # create the colours for the graph
    color_palette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))(ntrt)
    # create the graph
    plt <- ggplot2::ggplot(des, ggplot2::aes(x = col, y = row, fill = trt)) + ggplot2::geom_tile(colour = "black") +
        ggplot2::geom_text(aes(label = trt)) +
        ggplot2::theme_bw() + ggplot2::scale_fill_manual(values = color_palette, name = "Treatment")

    print(plt)

    return(des)

}
