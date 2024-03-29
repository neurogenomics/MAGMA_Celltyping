#' List SNPs-to-genes mapping files
#' 
#' List paths to all SNPs-to-genes mapping files generated by
#' \link[MAGMA.Celltyping]{map_snps_to_genes}.
#' 
#' @param save_dir Directory to recursively search for matching files in.
#' @param verbose Print messages.
#' @inheritParams base::list.files
#' 
#' @return Named list of paths.
#' 
#' @export
list_snps_to_genes_files <- function(save_dir,
                                     pattern = "*.genes.out$",
                                     verbose = TRUE){
    gene_files <- list.files(path = save_dir, 
                              pattern = pattern,
                              recursive = TRUE, 
                              full.names = TRUE)
    names(gene_files) <-basename(dirname(dirname(dirname(gene_files))))
    messager(length(gene_files),"files found.",v=verbose)
    return(gene_files)
}