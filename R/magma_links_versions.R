#' Get MAGMA versions
#' 
#' Get the version numbers from each MAGM executable download link. 
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
                                 unique_only = TRUE,
                                 filter_v = TRUE,
                                 verbose = TRUE){
    if(!is.character(links)) return(NULL)
    versions <- sapply(seq_len(length(links)),function(i){
      if(endsWith(links[[i]],".zip")){
        strsplit(links[[i]],"_")[[1]][2]
      } else{
        strsplit(names(links)[i],"_")[[1]][2] 
      }
    })
    versions <- gsub(".zip","",versions)
    if(unique_only){
        versions <- unique(versions)
    } 
    #### Newest versions will be listed at the bottom of the page ####
    latest_version <- rev(versions)[1]
    #### Check whether user-requested version exists ####
    if(!is.null(version)){
        version <- paste0("v",gsub("^v","",version))
        if(version %in% versions){
            versions <- version
        } else {
            messager("Requested version",paste0("(",version,")"),"not found.",
                     "Using latest version",latest_version,"instead.",
                     v=verbose)
            return(latest_version)
        }
    }
    if(filter_v){
        versions <- versions[startsWith(versions,"v")]    
    }
    if(return_all) versions else latest_version 
}
