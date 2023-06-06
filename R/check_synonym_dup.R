check_synonym_dup <- function(synonym_dup){ 
  
  opts <- eval(formals(map_snps_to_genes)$synonym_dup)
  if(is.null(synonym_dup)){
    synonym_dup <- opts[1]
  } else {
    synonym_dup <- synonym_dup[1]
    if(!synonym_dup %in% opts){
      stopper("`synonym_dup` must be one of:",
              paste("\n -",opts, collapse = ""))
    }
  }
  return(synonym_dup)
}