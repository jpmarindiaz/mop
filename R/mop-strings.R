#' @export
str_tpl_format <- function(tpl, l){
  if("list" %in% class(l)){
    listToNameValue <- function(l){
      mapply(function(i,j) list(name = j, value = i), l, names(l), SIMPLIFY = FALSE)
    }
    f <- function(tpl,l){
      gsub(paste0("{",l$name,"}"), l$value, tpl, fixed = TRUE)
    }
    return( Reduce(f,listToNameValue(l), init = tpl))
  }
  if("data.frame" %in% class(l)){
    myTranspose <- function(x) lapply(1:nrow(x), function(i) lapply(l, "[[", i))
    return( unlist(lapply(myTranspose(l), function(l, tpl) str_tpl_format(tpl, l), tpl = tpl)) )
  }
}


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

#' @export
extract_inside_parenthesis <- function(s){
  str_extract(s,"(?<=\\().*?(?=\\))")
}

#' @export
extract_inside_quotes <- function(s, quoteMark = '"'){
  if(quoteMark == "'") out <- str_extract(s,"(?<=').*?(?=')")
  else out <- str_extract(s,'(?<=").*?(?=")')
  out
}

#' @export
extract_between_chars <- function(s,chr1,chr2 = NULL){
  chr2 <- chr2 %||% chr1
  pattern <- paste0("(?<=",chr1,").*?(?=",chr2,")")
  str_extract(s,pattern)
}

#' @export
extract_numbers <- function(s, decimal = ".", thousands = ","){
  #remove_punctuation <- gsub('[[:punct:] ]+',' ',s)
  # unlist(s)
  # s <- unlist(strsplit(unlist(s), "[^0-9]+"))
  # unlist(na.omit(as.numeric(s)))
  unique(na.omit(as.numeric(unlist(strsplit(unlist(s), "[^0-9]+")))))
}

#' @export
extract_last_word <- function(s, n = 1){
  #stringi::stri_extract_last_words()
  word(trim_spaces(trim_punct(s)),-n)
}


