

#' @export
contains_word <- function(){

}


#' @export
match_replace <- function(v,dic, force = TRUE){
  matches <- dic[[2]][match(v,dic[[1]])]
  out <- matches
  if(!force)
    out[is.na(matches)] <- v[is.na(matches)]
  out
}


#' @export
fct_recode_df <- function(d,col,codes){
  left_join(d[col],codes, by=col) %>% .$to
}

#' @export
fct_to_chr <- function(d){
  d %>% mutate_if(is.factor, as.character)
}

date_to_chr <- function(d){
  d %>% mutate_if(is.Date, as.character)
}

#' @export
discard_all_na_rows <- function(d){
  d %>% filter(apply(., 1, function(x) !all(is.na(x))))
}

#' @export
discard_any_na_rows <- function(d){
  d %>% filter(apply(., 1, function(x) !any(is.na(x))))
}

#' @export
discard_all_na_cols <- function(d){
  f <- function(x)!all(is.na(x))
  d %>% keep(f)
}

#' @export
which_all_na_rows <- function(d, col = NULL, na = c(NA)){
  if(!is.null(col)) i <- d[[col]]
  else i <- 1:nrow(d)
  idx <- apply(d, 1, function(d) all(is.na(d)))
  i[idx]
}

#' @export
which_any_na_rows <- function(d, col = NULL, na = c(NA)){
  if(!is.null(col)) i <- d[[col]]
  else i <- 1:nrow(d)
  idx <- apply(d, 1, function(d) any(is.na(d)))
  i[idx]
}

#' @export
which_all_na_cols <- function(d){
  keep(d,~all(is.na(.))) %>% names()
}

#' @export
which_any_na_cols <- function(d, col = NULL, na = c(NA)){
  keep(d,~any(is.na(.))) %>% names()
}

#' @export
na_to_empty_chr <- function(df, empty = c(" ")){
  df[is.na(df)] <- ""
  df[df %in% empty] <- ""
  df
}

#' @export
empty_chr_to_na <- function(df){
  df[df==""] <- NA
  df
}

#' @export
which_not_in_dic <- function(d, dic, cols = NULL, as_idx = FALSE){
  cols <- cols %||% names(d)
  if(is.numeric(cols)) cols <- names(d)[cols]
  if(length(cols)!= ncol(dic)) stop("Number of cols and diccols must be the same")
  dic <- unite_(dic, "custom_id", names(dic))
  dd <- unite_(d,"custom_id",cols)
  wh <- which_not_in(dd$custom_id,dic$custom_id)
  ids <- match(wh,dd$custom_id)
  if(as_idx)
    return(ids)
  d %>% slice(ids)
}




