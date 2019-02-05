#' @export
summarize_str <- function(d){
  # n.vals, n.null, n.na, n.givenVals,...
  num_vars <- summarize_df_num(d)
  if(!is.null(num_vars)) num_vars$.type <- "num"
  cat_vars <- summarize_df_cat(d)
  if(!is.null(cat_vars)) cat_vars$.type <- "cat"
  s <- bind_rows(cat_vars,num_vars)
  s <- s %>%
    mutate(variable =  factor(variable, levels = names(d))) %>%
    arrange(variable)
  fct_to_chr(s)
}

#' @export
summarize_df_num <- function(d){
  d_num <- keep(d,is.numeric)
  if(ncol(d_num) == 0) return(NULL)
  pct.missing <- function(v) 100*sum(is.na(v))/length(v)
  n.missing <- function(v) sum(is.na(v))
  n.unique <- function(v) length(unique(v))
  s_num <- summarise_all(d_num,
                         funs(n.missing,pct.missing, n.unique,
                              min,max,mean,median))
  #x <- unlist(transpose(s_cat)[[1]])
  x <- as_vector(s_num)
  s_num <- tibble(var = names(x),value = x)
  if(ncol(d_num) == 1) s_num$var <- paste(names(d),s_num$var,sep = "_")
  #s <- s_num %>% separate(var, c("variable","metric"),sep = "_")
  s <- s_num %>% extract(var, c("variable","metric"), "(.*)_(.*)$")
  summary_num <- s %>% spread(metric,value)
  summary_num
}

#' @export
summarize_df_cat <- function(d){
  d_chr <- fct_to_chr(keep(d,~!is.numeric(.)))
  if(ncol(d_chr) == 0) return(NULL)
  pct.missing <- function(v) 100*sum(is.na(v))/length(v)
  n.missing <- function(v) sum(is.na(v))
  n.unique <- function(v) length(unique(v))
  unique.vals <- function(v){
    vals <- sort(unique(v))
    f <- function(x){
      if(nchar(x)>20)
        x <- paste0(strtrim(x,20),"[...]")
      x
    }
    vals <- map_chr(vals,f)
    paste(vals,collapse = "|")
  }
  s_cat <- summarise_all(d_chr,
                         funs(n.missing,pct.missing, n.unique,unique.vals))
  #x <- as_vector(s_cat)
  x <- unlist(transpose(s_cat)[[1]])
  s_cat <- tibble(var = names(x),value = x)
  if(ncol(d_chr) == 1) s_cat$var <- paste(names(d),s_cat$var,sep = "_")
  #s <- s_cat %>% separate(var, c("variable","metric"),sep = "_")
  s <- s_cat %>% extract(var, c("variable","metric"), "(.*)_(.*)$")
  summary_cat <- s %>% spread(metric,value)
  summary_cat <- as_tibble(map_at(summary_cat,
                                      c("n.missing","pct.missing","n.unique"),
                                      as.numeric))
  summary_cat
}

#' @export
unique_cats <- function(d, cols = NULL, as_long = TRUE){
  cols <- cols %||% names(d)
  d <- d[cols]
  d <- fct_to_chr(keep(d,~!is.numeric(.)))
  #dd <- map(d,~as.list(unique(.)))
  dd <- map(d,unique)
  vars <- tibble(variables = names(d))
  vars$values <- dd
  if(as_long)
    return(unnest(vars))
  vars
}

