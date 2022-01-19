#' Load RData 
#' 
#' Load a .rda file directly into a specific variable, 
#' This avoids the issue of not knowing what the variable name will be when 
#' loadings .rda files the usual way (\code{load}). 
#' 
#' @param fileName Path to .rda file. 
#' 
#' @keywords internal
load_rdata <- function(fileName) {
    load(fileName)
    get(ls()[ls() != "fileName"])
}
