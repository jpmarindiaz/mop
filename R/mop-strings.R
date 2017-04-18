
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

