#functions cvn
normalize <- function(x) { 
  x <- sweep(x, 2, apply(x, 2, min)) 
  sweep(x, 2, apply(x, 2, max), "/") 
} 


length.unique <- function(p)
{
  length(unique(p))
}
shannon.entropy <- function(p)
{
  if (min(p) < 0 || sum(p) <= 0)
    return(NA)
  p.norm <- p[p>0]/sum(p)
  -sum(log2(p.norm)*p.norm)
}

shannon.entropy.num <- function(p, distribution=TRUE){
  if (distribution){
    if (min(p) < 0 || sum(p) <= 0)
      return(NA)
    dens <- p/sum(p)
    dens <- dens[dens>0]
  }    
  else{
    h <- hist(p) 
    if (min(h$counts) < 0 || sum(h$counts) <= 0)
      return(NA)
    dens <- h$counts[h$counts>0]/sum(h$counts)
  }
  -sum(log2(dens)*dens) / log2(length(p))
}


periodicity <- function(y){
  spec <- spectrum(y)
  idx <- which(max(spec$spec)==spec$spec)
  period = 1/spec$freq[idx]
  tt <- spec$spec
  localmax <- which(diff(sign(diff(tt)))==-2)+1 #local maxima indexes second derivative
  level <- 1/length(localmax)
  periodicity <- list('level' = level, 'period'=period )
  periodicity
}


stat.desc.fac <- function(data)
{
  nm <- names(data)
  desfac <- as.data.frame(sapply(names(data), function (nms){
    #nms <- "Género"
    col <- data[,nms]
    #length(col)
    #nbr.val <- ifelse(any(p$Var1 == ""),nrow(p)-1,nrow(p))
    nbr.val <- length(col[col != ""])
    nbr.empty <- length(col[col == ""])    
    # Remove empty values
    col <- col[col!=""]
    col <- factor(col)
    nbr.unq <- length(unique(col))
    ptable <- as.data.frame(table(col))
    p <- as.data.frame(prop.table(table(col)))
    
    nbr.entropy <- ifelse(nrow(p)>1, shannon.entropy(p$Freq)/log2(nrow(p)),0)
    mst.commons <- as.character(p$col[which(p$Freq==max(p$Freq))])
    mst.common.nbr <- length(mst.commons)
    if(mst.common.nbr == 1){ 
      mst.common.nbr <- ptable$Freq[which(ptable$Freq==max(ptable$Freq))]
    }
    mst.common <- paste(mst.commons, collapse = " | ")
    if(nchar(mst.common)>50){mst.common <- paste0(mopStrChop(mst.common,end=50),"...") }
    lst.commons <- as.character(p$col[which(p$Freq==min(p$Freq))])
    lst.common.nbr <- length(lst.commons)
    lst.common <- paste(lst.commons, collapse = " | ")
    if(nchar(lst.common)>50){lst.common <- paste0(mopStrChop(lst.common,end=50),"...") }
    nbr.na <- sum(is.na(col))
    c(nbr.val,nbr.empty,nbr.unq,nbr.entropy,mst.common, mst.common.nbr,lst.common, lst.common.nbr, nbr.na) 
  }))
  desfac <- cbind(c("nbr.val","nbr.empty","nbr.unq","entropy","mst.common","mst.common.qt","lst.common","lst.common.val.nbr","nbr.na"),desfac)
  names(desfac) <- c("vars",names(data) )
  desfac
}  



