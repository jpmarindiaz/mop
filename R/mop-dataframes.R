
#' @export
squeeze_rows <- function(d, by = NULL){
  if(is.null(by)) d$.id <- 1:nrow(d)
  else d$.id <- by
  dlong <- d %>% gather(key,vals,-.id) %>% discard_any_na_rows()
  dlong %>% spread(key,vals)
}


#' @export
spread_every <- function(v,n,into = NULL){
  #n = 3
  #v <- rep(letters[1:n],5)
  col_name <- deparse(substitute(v))
  d <- data_frame(v)
  d$idx <- flatten_int(map(seq_len(length(v) %/% n),rep,n))
  out <- d %>% slice_rows("idx") %>% by_slice( function(x)x$v, .collate="cols")
  out$idx <- NULL
  if(!is.null(into))
    names(out) <- into
  out
}


#' @export
contains_word <- function(){
NULL
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
match_replace_approx <- function(v,dic, max_dist = 0.1, method = "jw", force = TRUE){
  if(nrow(dic) == 0) stop("Empty dictionary")
  x <- data_frame(x = v, .id = 1:length(x))
  names(x)[1] <- names(dic)[1]
  y <- stringdist_left_join(x, dic, method = method,max_dist = max_dist,
                            distance_col = ".dist")
  if(!".dist" %in% names(y)) y$.dist <- NA
  y <- y %>% arrange(.id, .dist)
  names(y)[1] <- names(x)[1]
  if(force){
    yy <- y %>% group_by(.id) %>% slice(1) %>% ungroup()

    return( yy %>% select(-.id, -.dist))
  }
  y
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
discard_all_empty_rows <- function(d){
  d %>% filter(apply(., 1, function(x) !all(x == "")))
}

#' @export
discard_any_empty_rows <- function(d){
  d %>% filter(apply(., 1, function(x) !any(x == "")))
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
keep_all_na_rows <- function(d){
  d %>% filter(apply(., 1, function(x) all(is.na(x))))
}

#' @export
keep_any_na_rows <- function(d){
  d %>% filter(apply(., 1, function(x) any(is.na(x))))
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




