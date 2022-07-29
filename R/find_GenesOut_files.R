#' Find genes.out files
#' 
#' Search for MAGMA output files (genes.out) in a given directory.
#' Support function for \link[MAGMA.Celltyping]{get_driver_genes}. 
#' 
#' @param GenesOut_dir Directory to search in. 
#' @param verbose Print messages. 
#' 
#' @keywords internal 
find_GenesOut_files <- function(GenesOut_dir,
                                verbose = TRUE) { 
    magma_GenesOut_file <- list.files(GenesOut_dir, 
                                      ".genes.out",
                                      recursive = TRUE, full.names = TRUE
    )
    messager(length(magma_GenesOut_file), "genes.out file(s) found.", 
             v=verbose)
    return(magma_GenesOut_file)
}
