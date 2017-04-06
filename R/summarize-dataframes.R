#' @export
summarize_str <- function(d){

  num_vars <- summarize_df_num(d)
  num_vars$.type <- "num"
  cat_vars <- summarize_df_cat(d)
  cat_vars$.type <- "cat"
  s <- bind_rows(cat_vars,num_vars)
  s <- s %>%
    mutate(variable =  factor(variable, levels = names(d))) %>%
    arrange(variable)
  fct_to_chr(s)
}

#' @export
summarize_df_num <- function(d){
  d_num <- keep(d,is.numeric)
  pct.missing <- function(v) sum(is.na(v))/length(v)
  n.missing <- function(v) 100*sum(is.na(v))
  n.unique <- function(v) length(unique(v))
  s_num <- summarise_all(d_num,
                         funs(n.missing,pct.missing, n.unique,
                              min,max,mean,median))
  s_num <- as_vector(s_num)
  s_num <- data_frame(var = names(s_num),value = s_num)
  s <- s_num %>% separate(var, c("variable","metric"),sep = "_")
  summary_num <- s %>% spread(metric,value)
  summary_num
}

#' @export
summarize_df_cat <- function(d){
  d_chr <- fct_to_chr(keep(d,~!is.numeric(.)))
  pct.missing <- function(v) sum(is.na(v))/length(v)
  n.missing <- function(v) 100*sum(is.na(v))
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
  #s_cat <- gather(s_cat)
  #s_cat <- empty_chr_to_na(s_cat)
  s_cat <- as_vector(s_cat)
  s_cat <- data_frame(var = names(s_cat),value = s_cat)
  s <- s_cat %>% separate(var, c("variable","metric"),sep = "_")
  summary_cat <- s %>% spread(metric,value)
  summary_cat <- as_data_frame(map_at(summary_cat,
                                      c("n.missing","pct.missing","n.unique"),
                                      as.numeric))
  summary_cat
}

