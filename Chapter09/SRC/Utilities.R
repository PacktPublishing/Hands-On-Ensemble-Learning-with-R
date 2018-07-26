# to.dendogram function is from the source 
# https://stats.stackexchange.com/questions/2344/best-way-to-present-a-random-forest-in-a-publication


to.dendrogram <- function(dfrep,rownum=1,height.increment=0.1){
  if(dfrep[rownum,'status'] == -1){
    rval <- list()
    attr(rval,"members") <- 1
    attr(rval,"height") <- 0.0
    attr(rval,"label") <- dfrep[rownum,'prediction']
    attr(rval,"leaf") <- TRUE
  }else{##note the change "to.dendrogram" and not "to.dendogram"
    left <- to.dendrogram(dfrep,dfrep[rownum,'left daughter'],height.increment)
    right <- to.dendrogram(dfrep,dfrep[rownum,'right daughter'],height.increment)
    rval <- list(left,right)
    attr(rval,"members") <- attr(left,"members") + attr(right,"members")
    attr(rval,"height") <- max(attr(left,"height"),attr(right,"height")) + height.increment
    attr(rval,"leaf") <- FALSE
    attr(rval,"edgetext") <- dfrep[rownum,'split var']
    #To add Split Point in Dendrogram
    #attr(rval,"edgetext") <- paste(dfrep[rownum,'split var'],"\n<",round(dfrep[rownum,'split point'], digits = 2),"=>", sep = " ")
  }
  class(rval) <- "dendrogram"
  return(rval)
}

plot_RF <- function(RF){
  n <- RF$ntree
  for(i in 1:n){
    tt <- getTree(RF,i,labelVar = TRUE)
    dt <- to.dendrogram(tt)
    plot(dt,center=TRUE,edgePar=list(t.cex=1,p.col=NA,p.lty=0),
         yaxt='n',horiz=TRUE)
    print(i)
  }
}
