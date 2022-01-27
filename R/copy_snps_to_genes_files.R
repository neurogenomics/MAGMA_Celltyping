#' Copy MAGMA gene mapping files to new folder
#' 
#' Copy SNPs-to-genes mapping files (\emph{.genes.out}) produced by 
#' \link[MAGMA.Celltyping]{map_snps_to_genes} to a new folder. 
#' Importantly, this function maintains the file naming and folder structure 
#' conventions necessary to run other downstream 
#' \pkg{MAGMA.Celltyping} functions.
#' 
#' @param gene_files Full paths to gene mapping mapping files.
#' @param save_dir Where to save the gene mapping files to.
#' @param overwrite Whether to overwrite gene mapping files that already
#'  exist in \code{save_dir} (Default: \code{FALSE}).
#' 
#' @return Named character list
#' 
#' @keywords internal 
copy_snps_to_genes_files <- function(gene_files, 
                                     save_dir = "MAGMA_Files",
                                     overwrite = FALSE){
    requireNamespace("parallel")
    gene_files2 <- parallel::mclapply(gene_files, function(x){
        message_parallel(basename(x))
        new_file <- file.path(save_dir,
                              basename(dirname(x)),
                              basename(x))
        if(file.exists(new_file) && overwrite == FALSE){
            message_parallel("Skipping: File already exists.")
            return(new_file)
        } else {
            dir.create(dirname(new_file),
                       showWarnings = FALSE, recursive = TRUE)
            file.copy(x, new_file, overwrite = overwrite)
        }
        return(new_file)
    })
    return(gene_files2)
}
