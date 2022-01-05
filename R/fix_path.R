fix_path <- function(x){
  x <- path.expand(x)
  if(get_os()=='Windows'){
    x <- normalizePath(x, mustWork = FALSE)
  }
  return(x)
}
