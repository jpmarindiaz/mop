
#' @export
named_list_to_df <- function(x){
  data_frame(name = names(x), x)
}
