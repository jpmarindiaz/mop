
#' @export
copy_clipboard <- function(x, row.names = FALSE, col.names = TRUE, ...){
  sys <- Sys.info()[['sysname']]
  if(sys == "Linux"){
    con <- pipe("xclip -selection clipboard -i", open="w")
    write.table(x, con, sep=sep, row.names=row.names, col.names=col.names, na="", ...)
    close(con)
  }
  if(sys == "Darwin"){
    clip <- pipe("pbcopy", "w")
    write.table(x, file = clip, sep="\t", row.names=row.names, col.names=col.names, na="",...)
    close(clip)
  }
  if(sys == "Windows"){
    write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,na="",...)
  }
  message("Copied dataframe to clipboard")
}
