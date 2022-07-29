#' Import magma files as tables
#' 
#' Import magma \emph{genes.out} and \emph{genes.raw} 
#' output files as a nested list of \link[data.table]{data.table}s.
#' @param local_files A list of magma files, nested by GWAS ID.
#' @inheritParams read_magma_genes_out
#' @keywords internal
import_magma_files_tables <- function(local_files,
                                      verbose = TRUE){
    mapply(local_files, 
           SIMPLIFY = FALSE,
           FUN=function(x){
               mf <- list()
               if(!is.na(x["genes.out"])){
                   mf[["genes.out"]] <- read_magma_genes_out(
                       path = x["genes.out"],
                       verbose = verbose)    
               } 
               if(!is.na(x["genes.raw"])){
                   mf[["genes.raw"]] <- read_magma_genes_raw(
                       path = x["genes.raw"],
                       verbose = verbose)
               }
               return(mf)
           })
}
