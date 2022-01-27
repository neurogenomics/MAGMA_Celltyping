#' Get actual path 
#' 
#' When given a file path, check that it exists and if not, search for
#'  similarly named files and return those instead. 
#' This is important because sometimes MAGMA adds a .txt. at the end of 
#' output files (especially on Windows). 
#' 
#' @param path File path. 
#' 
#' @keywords internal 
get_actual_path <- function(path){
    if(file.exists(path)) return(path)
    actual_path <- list.files(dirname(path), basename(path),
                              full.names = TRUE)
  if(length(actual_path)>0) return(actual_path) else  return(path)
}
