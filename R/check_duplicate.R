check_duplicate <- function(duplicate){ 
  
  opts <- eval(formals(map_snps_to_genes)$duplicate)
  if(is.null(duplicate)){
    duplicate <- opts[1]
  } else {
    duplicate <- duplicate[1]
    if(!duplicate %in% opts){
      stopper("`duplicate` must be one of:",
              paste("\n -",opts, collapse = ""))
    }
  }
  return(duplicate)
}