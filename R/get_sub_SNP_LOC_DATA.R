#' Download genome ref: piggyback
#'
#' Download and decompress the genome reference needed to run
#' \link[MAGMA.Celltyping]{map_snps_to_genes},
#' If the file already exists, it will simply return the path.
#' 
#' @inheritParams get_data
#' @source 
#' \code{ 
#' URL <- paste(
#'     "https://github.com/neurogenomics/MAGMA_Celltyping/raw/master/",
#'     "data/sub_SNP_LOC_DATA.rda", sep="/")
#' tmp <- file.path(tempdir(),basename(URL))  
#' piggyback::pb_upload(file = tmp,
#'                      repo = "neurogenomics/MAGMA_Celltyping",
#'                      overwrite = TRUE)
#' }
#' @keywords internal
#' @importFrom tools R_user_dir
get_sub_SNP_LOC_DATA <- function(storage_dir = tools::R_user_dir(
                                    package = "MAGMA.Celltyping",
                                    which = "cache"
                                )) {
     tmp <- get_data(fname = "sub_SNP_LOC_DATA.rda", 
                     storage_dir = storage_dir)
     sub_SNP_LOC_DATA <- EWCE::load_rdata(fileName = tmp)
     return(sub_SNP_LOC_DATA)
}
