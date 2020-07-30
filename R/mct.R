library(agricolae)
reorder<-function(inV){
    collapsed <- paste(inV,sep="",collapse = "")
    u <- unique(strsplit(collapsed,"")[[1]])
    if(length(u)<2){
        return(inV)
    }
    u <- u[order(u)]
    m <- matrix(nrow=NROW(inV),ncol=length(u))
    m[]<-F
    for(i in 1:length(inV)){
        s <- strsplit(inV[i],"")[[1]]
        index <- match(s,u)
        m[i,index] <- T
    }
    for(i in 1:(length(u)-1)){
        firstColT <- match(T,m[,i])[1] #first row with true in current column
        firstT <- match(T,rowSums(m[,i:length(u)] > 0))[1] #first row with true in rest
        if(firstT < firstColT){
            colT <- match(T,m[firstT,i:length(u)])[1]
            colT <- colT + i - 1 #correct index for leftout columns in match
            tmp <- m[,colT]
            m[,colT] <- m[,i]
            m[,i] <- tmp
        }
    }
    res <- vector(mode = "character", length=length("trt"))
    for(i in 1:length(inV)){
        l <- u[m[i,]]
        res[i] <- paste(l,sep="",collapse = "")
    }
    return(res)
}



fit <- lm(Sepal.Length ~ Species, data = iris)
a <- HSD.test(fit, "Species", group=T, console=F)$groups
a <- a[rev(rownames(a)),] #order the result the way you want
a$M <- reorder(as.character(a$M))
