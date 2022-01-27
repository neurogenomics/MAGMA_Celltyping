#' Get MAGMA links
#' 
#' Get links to MAGMA executables in the official MAGMA archives. 
#' 
#' @param latest_only Only return the latest version of MAGMA.
#' @param os Operating System.
#' @param version Desired MAGMA version. 
#' @param use_local Use a copy of the MAGMA file links, which may be out of date
#' but will always be available. 
#' @param return_table Return the results in table format. 
#' @param verbose Print messages.
#' 
#' @keywords internal
magma_links <- function(latest_only = TRUE,
                        os = get_os(),
                        version = NULL,
                        use_local = TRUE,
                        return_table = FALSE,
                        verbose = TRUE) {
    latest <- NULL;
    #### Avoid issues with subset when colnames same as variable names ####
    OS <- os;
    VERSION <- if(is.null(version)) {
        version 
    } else {
        paste0("v",gsub("^v","",version))
    }
    #### Try to search archives ####
    ## If this fails, use a stored backup of the URLs
    if(use_local){
        meta <- MAGMA.Celltyping::magma_links_stored
    } else {
        meta <- tryCatch(expr = {
            magma_links_gather()
        }, error = function(e) MAGMA.Celltyping::magma_links_stored)
    }  
    #### Filter by OS ####
    if (!is.null(OS)) {  
        meta <- subset(meta, os==OS)
    }
    #### Filter by MAGMA version ####
    if (!is.null(VERSION) && (!latest_only)) {
        meta2 <- subset(meta, version==as.character(VERSION))
        #### Check that any versions left #####
        if(nrow(meta2)==0){
            messager("No versions found matching criterion.")
        } else {
            meta <- meta2
        }
    } 
    if(latest_only) {
        meta <- subset(meta, latest==TRUE)
    }
    #### Remove duplicates ####
    meta <- meta[!duplicated(meta$name),] 
    #### Return table or dictionary ####
    if(return_table){
        return(meta)
    }else {
        dict <- stats::setNames(meta$link, 
                                meta$name)
        return(dict)
    }  
}
