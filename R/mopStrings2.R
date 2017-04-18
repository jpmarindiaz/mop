
##############

#' mopWord
#' @name mopWord
#' @description mopWord
#' @param s string or dataframe
#' @param pos string, "first" or "last"
#' @return string
#' @export
#' @examples \dontrun{
#' }
mopWord <- function(s, pos ="first",cols=NULL){
  getWord <- function(pos){
    #str <- "Caserío La Mesa-Veredas La Mesa, La Danta y Mulatos"
    #stringi:::stri_extract_last(str, regex="\\w+")
    ##stringi:::stri_extract_last(s, regex="[:alnum:]+$")
    #str_extract(str, '\\w+$')
    function(str){

      if(pos=="first"){
        out <- word(str,1)
      }
      else if(pos=="last"){
        out <- word(str,-1)
      }

      # else if(!pos %in% c("first","last") ){
      #   stop("Specify pos='first' or pos='last'")
      # }
      out
    }
  }
  f <- getWord(pos = pos)
  if(class(s) =="factor") s <- as.character(s)
  if(class(s) == "character"){
    out <- f(s)
  }
  if("data.frame" %in% class(s)){
    colNums <- match(cols,names(s)) %||% 1:ncol(s)
    df <- as.data.frame(s[,colNums])
    names(df) <- names(s)[colNums]
    out <- as.data.frame(sapply(df, f))
    # df <- tbl_df(s)
    # #cols <- c("fuente","implicado")
    # colNums <- match(cols,names(df)) %||% 1:ncol(df)
    # out <- df %>%
    #         select(colNums) %>%
    #         rowwise() %>%
    #         mutate_each(funs(f))
  }
  out
}

#' mopSplitFixedPattern
#' @name mopSplitFixedPattern
#' @description mopSplitFixedPattern
#' @param string
#' @return string
#' @export
#' @examples \dontrun{
#' }
mopSplitFixedPattern <- function(s, pattern, splitLength = 2, cols=NULL){
  stringSplitFun <- function(pattern, splitLength){
    function(str){
      as.data.frame(stringr::str_split_fixed(str,pattern,splitLength))
    }
  }
  f <- stringSplitFun(pattern = pattern, splitLength = splitLength)
  #library(pryr)
  #unenclose(f)
  if(class(s) =="factor") s <- as.character(s)
  if(class(s)=="character"){
    out <- f(s)
    names(out) <- paste("COL",1:splitLength, sep=".")
  }
  if("data.frame" %in% class(s)){
    colNums <- match(cols,names(s)) %||% 1:ncol(s)
    if(length(colNums)==1){
      out <- f(s[,colNums])
      names(out) <- paste(names(s)[colNums],1:splitLength, sep=".")
    } else{
      df <- as.data.frame(s[,colNums])
      names(df) <- names(s)[colNums]
      l <- lapply(df, f)
      out <- do.call(cbind,l)
    }
  }
  out
}

#' mopWhiteSpace
#' @name mopWhiteSpace
#' @description mopWhiteSpace
#' @param string
#' @return string
#' @export
#' @examples \dontrun{
#' }
mopWhiteSpace <- function(s, cols=NULL){
  trimWhite <- function(str){
    #str <- "  Caserío La Mesa-Veredas La Mesa, La Danta y Mulatos "
    stringr:::str_trim(str)
  }
  f <- trimWhite
  if(class(s) =="factor") s <- as.character(s)
  if(class(s)=="character"){
    out <- f(s)
  }
  if("data.frame" %in% class(s)){
    colNums <- match(cols,names(s)) %||% 1:ncol(s)
    df <- as.data.frame(s[,colNums])
    names(df) <- names(s)[colNums]
    out <- as.data.frame(sapply(df, f))
    #df <- tbl_df(s)
    #cols <- c("fuente","implicado")
    #colNums <- match(cols,names(df)) %||% 1:ncol(df)
    #out <- df %>%
    #  select(colNums) %>%
    #  rowwise() %>%
    #  mutate_each(funs(f))
  }
  out
}

#' mopStrChop
#' @name mopStrChop
#' @description mopStrChop
#' @param string
#' @return string
#' @export
#' @examples \dontrun{
#' }
mopStrChop <- function(s, start = 1, end = 2, cols=NULL){
  strChop <- function(start,end){
    function(str){
      #str <- "Caserío La Mesa-Veredas La Mesa, La Danta y Mulatos "
      substr(str, start = start, stop = end)
    }
  }
  f <- strChop(start = start, end = end)
  if(class(s) =="factor") s <- as.character(s)
  if(class(s)=="character"){
    out <- f(s)
  }
  if("data.frame" %in% class(s)){
    colNums <- match(cols,names(s)) %||% 1:ncol(s)
    df <- as.data.frame(s[,colNums])
    names(df) <- names(s)[colNums]
    out <- as.data.frame(sapply(df, f))
    #df <- tbl_df(s)
    #cols <- c("fuente","implicado")
    #colNums <- match(cols,names(df)) %||% 1:ncol(df)
    #out <- df %>%
    #  select(colNums) %>%
    #  rowwise() %>%
    #  mutate_each(funs(f))
  }
  out
}

#' mopAccents
#' @name mopAccents
#' @description mopAccents
#' @param string
#' @return string
#' @export
#' @examples \dontrun{
#' }
mopAccents <- function(s, cols=NULL){
  f <- removeAccents
  if(class(s) =="factor") s <- as.character(s)
  if(class(s)=="character"){
    out <- f(s)
  }
  if("data.frame" %in% class(s)){
    colNums <- match(cols,names(s)) %||% 1:ncol(s)
    df <- as.data.frame(s[,colNums])
    names(df) <- names(s)[colNums]
    out <- as.data.frame(sapply(df, f))
    #df <- tbl_df(s)
    ##cols <- c("fuente","implicado")
    #colNums <- match(cols,names(df)) %||% 1:ncol(df)
    #out <- df %>%
    #  select(colNums) %>%
    #  rowwise() %>%
    #  mutate_each(funs(f))
  }
  out
}

#' mopDictionaryMatch
#' @name mopDictionaryMatch
#' @description mopDictionaryMatch
#' @param string
#' @return string
#' @export
#' @examples \dontrun{
#' }
mopDictionaryMatch <- function(s, dict, empty=c(""),cols=NULL){
  dicMatch <- function(dict, empty){
    function(str){
      #str <- "Caserío La Mesa-Veredas La Mesa, La Danta y Mulatos "
      dictionaryMatch(str, dict, empty)
    }
  }
  f <- dicMatch(dict, empty)
  if(class(s) =="factor") s <- as.character(s)
  if(class(s)=="character"){
    out <- f(s)
  }
  if("data.frame" %in% class(s)){
    colNums <- match(cols,names(s)) %||% 1:ncol(s)
    df <- as.data.frame(s[,colNums])
    names(df) <- names(s)[colNums]
    out <- as.data.frame(sapply(df, f))

    #dd <- tbl_df(s)
    ##cols <- c("fuente","implicado")
    #colNums <- match(cols,names(dd)) %||% 1:ncol(df)
    #out <- dd %>%
    #  select(colNums) %>%
    #  rowwise() %>%
    #  mutate_each(funs(f))
  }
  out
}

#' mopWordInAorB
#' @name mopWordInAorB
#' @description mopWordInAorB
#' @param string
#' @return string
#' @export
#' @examples \dontrun{
#' }
mopWordInAorB <- function(s, a, b, aname = "A", bname = "B", cols=NULL){

  #s <- "Ana"
  #s <- c("ana","carlos","edu")
  #s <- data.frame(x=c("ana","carlos","edu"), y= c("juan","carlota","fred"))
  #path <- system.file("aux", package = "mopper")
  #a <- read.csv(file.path(path, "nombres-hombre.csv"))[,1]
  #b <- read.csv(file.path(path, "nombres-mujer.csv"))[,1]

  wordAorB <- function(a, b, aname = "A", bname = "B"){
    function(w){
      #str <- "Caserío La Mesa-Veredas La Mesa, La Danta y Mulatos "
      isWordInAorB(w, a, b, aname, bname)
    }
  }

  ## OJO f tiene que ser vectorizada
  f <- wordAorB(a, b, aname, bname)

  if(class(s) =="factor") s <- as.character(s)
  if(class(s)=="character"){
    out <- f(s)
  }
  if("data.frame" %in% class(s)){
    colNums <- match(cols,names(s)) %||% 1:ncol(s)
    df <- as.data.frame(s[,colNums])
    names(df) <- names(s)[colNums]
    out <- as.data.frame(sapply(df, f))
  }
  out
}

#' @export
twoLines <- function(str){
  l <- lapply(str,function(s){
    out <- s
    if(nchar(s)>30){
      w <- word(s,start=1,end=-1)
      nw <- sapply(strsplit(s, " "), length)
      endw <- nw %/% 2
      ini <- word(s,start=1, end= endw)
      lst <-  word(s,start=endw+1, end=nw)
      out <- paste(ini,lst,sep="\n")
    }
    out
  })
  unlist(l)
}



