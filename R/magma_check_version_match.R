#' MAGMA check version match 
#' 
#' Check that the desired version of MAGMA is installed. 
#' @inheritParams magma_install
#' 
#' @keywords internal
magma_check_version_match <- function(desired_version,  
                                      verbose = TRUE){
    
    magma_x_list <- magma_executable(return_all = TRUE,
                                     verbose = FALSE)
    if(all(length(magma_x_list)==0)) {
        messager("No versions of MAGMA are available.",
                 "Returning NULL.",
                 v=verbose)
        return(NULL)
    }
    current_versions <- magma_installed_version(magma_x = magma_x_list,
                                                verbose = FALSE) 
    #### If MAGMA is installed, check which version #### 
    if (!desired_version %in% current_versions) {
        messager(
            "A different version of MAGMA",
            "than desired_version is currently installed:",
            paste(current_versions, collapse = ", "),
            "Set upgrade=TRUE to install desired_version as well:",
            desired_version,
            v = verbose
        ) 
        dest_magma <- rev(names(current_versions))[1] 
    } else {
        messager("The desired_version of MAGMA is currently installed:",
                 desired_version, v = verbose
        ) 
        dest_magma <- invert_dict(current_versions)[[desired_version]] 
    }   
    messager("Using:",basename(dirname(dest_magma)),
             v = verbose)
    return(dest_magma)
}
