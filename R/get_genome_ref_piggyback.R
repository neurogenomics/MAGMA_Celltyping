#' Download genome ref: piggyback
#'
#' Download and decompress the genome reference needed to run
#' \link[MAGMA.Celltyping]{map_snps_to_genes},
#' If the file already exists, it will simply return the path.
#' 
#' @param genome_ref_dir Folder to download reference to.
#' @param verbose Print messages.
#' @source 
#' \code{
#' genome_ref_path <- MAGMA.Celltyping::get_genome_ref(method = "magma")
#'
#' piggyback::pb_upload(file = genome_ref_path,
#'                      repo = "neurogenomics/MAGMA_Celltyping",
#'                      overwrite = TRUE)
#' }
#' @keywords internal
get_genome_ref_piggyback <- function(genome_ref_dir,  
                                     verbose = TRUE) {
    ##### Link to 1000 genomes reference data. ####
    zipfile <- sprintf("%s.zip", genome_ref_dir)
    #### Download file ####
    options(timeout = 60 * 5)
    get_data(paste0(basename(genome_ref_dir),".zip"),
             repo = "neurogenomics/MAGMA_Celltyping", 
             storage_dir = genome_ref_dir)
    #### Unzip file ####
    messager("Unzipping file.", v = verbose) 
    utils::unzip(
        zipfile = zipfile,
        exdir = genome_ref_dir
    )
}
