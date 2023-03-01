#' Fix path
#' 
#' Ensure a file/directory path can be understood by all OS types by expanding 
#' "~" to an absolute path, and standardized slash directions with 
#' \link[base]{normalizePath}. 
#' This is especially important for Windows.  
#' 
#' @param x File/directory path. 
#' @keywords internal
fix_path <- function(x){
  x <- path.expand(x)
  if(get_os()=='Windows'){
    # Remove a trailing slash to avoid errors on windows
    x <- gsub("\\/$", "", x)
    x <- normalizePath(x, mustWork = FALSE)
  }
  return(x)
}
