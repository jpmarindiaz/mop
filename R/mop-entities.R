
#' @export
clean_nit <- function(x){
  x <- gsub("\\.|,|\\s","",x)
  str_extract(x, "\\d{9}")
}

#' @export
clean_cc <- function(x){
  x <- gsub("\\.|,|\\s|[a-zA-Z]","",remove_accents(x))
  x
}

#' @export
clean_company_name <- function(x, context = "co"){
  y <- trim_spaces(x)
  if(context == "co"){
    y <- str_replace(y, "(?i)(SAS|S\\. A\\. S\\.)$(?-i)", "S.A.S.")
    y <- str_replace(y, "(?i)(SA|S\\. A\\.)$(?-i)", "S.A.")
    y <- str_replace(y, "(?i)(limitada|LTDA)$(?-i)", "LTDA.")
    y <- str_replace(y, "(?i)(SA ESP|SA. ESP)$(?-i)", "S.A. ESP.")
  }
  y
}

#' @export
mix_names_es <- function(x, combinations = NULL){
  combinations4 <- list(c(1,3),c(1,2,3),c(1,3,4))
  combinations3 <- list(c(1,2),c(1,2,3))
  combinations <- combinations %||% combinations4
  words <- str_extract_all(x,"\\w+")
  combs <- map(words,function(w){
    if(length(words) == 4)
      combinations <- combinations4
    if(length(words) == 3)
      combinations <- combinations3
    map_chr(combinations, function(comb){
      paste(w[comb] %>% remove_na(),collapse = " ")
    })
  })
  d <- tibble(name = x, combinations = combs)
  unnest(d) %>% distinct()
}
