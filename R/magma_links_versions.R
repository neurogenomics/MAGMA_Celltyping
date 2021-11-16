#' Get the available versions of MAGMA
#'
#' @source \href{https://ctg.cncr.nl/software/magma}{MAGMA website}
#' 
#' @returns Character vector of versions.
#' 
#' @keywords internal
#' @importFrom stringr str_split
magma_links_versions <- function(links,
                                 version = NULL,
                                 return_all = FALSE,
                                 verbose = TRUE){
    versions <- stringr::str_split(links, "_", simplify = TRUE)[,2]
    versions <- unique(gsub(".zip","",versions))
    #### Newest versions will be listed at the bottom of the page ####
    latest_version <- rev(versions)[1]
    #### Check whether user-requested version exists ####
    if(!is.null(version)){
        if(version %in% versions){
            versions <- version
        } else {
            messager("Requested version",paste0("(",version,")"),"not found.",
                     "Using latest version",latest_version,"instead.",
                     v=verbose)
            return(latest_version)
        }
    }
    if(return_all) versions else latest_version 
}
