get_actual_path <- function(path){
  ## Sometimes MAGMA adds a .txt. at the end
  actual_path <- list.files(dirname(path), basename(path),
                            full.names = TRUE)
  if(length(actual_path)>0) return(actual_path) else  return(path)
}
