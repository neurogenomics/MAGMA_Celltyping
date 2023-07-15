#' Import MAGMA files metadata
#' 
#' Import metadata from the SNP-to-gene mapping files available in 
#' \href{https://github.com/neurogenomics/MAGMA_Files_Public}{
#' MAMGA_Files_Public}.  
#' 
#' @param use_local Whether to use a local copy of the metadata.  
#' \itemize{
#' \item{\code{TRUE}}{Use the metadata included with \pkg{MAGMA.Celltyping}
#'  (less frequently updated).}  
#' \item{\code{FALSE}}{Import metadata from the GitHub repository 
#' (more frequently updated).}  
#' }  
#' @inheritParams import_magma_files
#' 
#' @keywords internal
import_magma_files_metadata <- function(file_types,
                                        use_local = TRUE,
                                        verbose = TRUE){
    if(isTRUE(use_local)){
        magma_files_metadata <- MAGMA.Celltyping::magma_files_metadata
    } else{
        magma_files_metadata <- data.table::fread(
            paste("https://github.com/neurogenomics/MAGMA_Files_Public",
                  "raw/master/metadata.csv", sep="/"),
            nThread = 1
        )
    } 
    return(magma_files_metadata)
    
    
    # #### Check what files are available #### 
    # magma_files <- github_list_files(
    #     user = "neurogenomics",
    #     repo = "MAGMA_Files_Public",
    #     branch = "master",
    #     query = paste(file_types, collapse = "|"),
    #     return_download_api = TRUE,
    #     verbose = verbose
    # )
}
