
#' @export
trim_spaces <- function(x){
  gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", x, perl=TRUE)
}

#' @export
trim_punct <- function (x){
  gsub("[[:punct:]]", "", x)
}

#' @export
remove_accents <- function(string){
  accents <- "àèìòùÀÈÌÒÙáéíóúýÁÉÍÓÚÝäëïöüÄËÏÖÜâêîôûÂÊÎÔÛñÑç"
  translation <- "aeiouAEIOUaeiouyAEIOUYaeiouAEIOUaeiouAEIOUnNc"
  chartr(accents, translation, string)
}


#' @export
create_slug <- function(x){
  x <- gsub("[^[:alnum:]]","-",x)
  x <- remove_accents(x)
  x <- tolower(x)
  x <- gsub("-+","-",x)
  x <- gsub("+-$","",x)
  x <- gsub("^-.","",x)
  x
}


#' @export
totitle <- function(x) {
  x <- tolower(x)
  s <- strsplit(x, " ")
  f <- function(str) paste(toupper(substring(str, 1,1)), substring(str, 2),
        sep="", collapse=" ")
  map_chr(s,f)
}

#' @export
nwords <- function(string, pseudo=FALSE){
  ifelse( pseudo,
          pattern <- "\\S+",
          pattern <- "[[:alpha:]]+"
  )
  str_count(string, pattern)
}


