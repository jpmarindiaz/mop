
#' @export
named_list_to_df <- function(x){
  tibble(name = names(x), unlist(x))
}
