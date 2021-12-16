import_magma_files_metadata <- function(file_types,
                                        use_local = TRUE,
                                        verbose = TRUE){
    if(use_local){
        magma_files_metadata <- MAGMA.Celltyping::magma_files_metadata
    } else{
        magma_files_metadata <- data.table::fread(
            file.path("https://github.com/neurogenomics/MAGMA_Files_Public",
                      "raw/master/metadata.csv"),
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
