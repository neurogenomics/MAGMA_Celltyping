#' Download files from GitHub
#' 
#' Download files stored in a public GitHub repository.
#' 
#' @param filelist List of URLs to files. 
#' @param save_dir Local directory to save files in. 
#' @param overwrite Overwrite existing local files. 
#' @param nThread Number of threads to parallelise downloads over. 
#' @param verbose Print messages. 
#' 
#' @keywords internal
github_download_files <- function(filelist,
                                  save_dir = tempdir(),
                                  overwrite = FALSE,
                                  nThread = 1,
                                  verbose = TRUE) {
    requireNamespace("stringr")
    requireNamespace("parallel")
    
    messager("Downloading", length(filelist),"files.", v = verbose)
    local_files <- unlist(parallel::mcmapply(filelist, 
                                             SIMPLIFY = FALSE,
                                             FUN = function(x) { 
        branch <- stringr::str_split(string = x, pattern = "/")[[1]][7]
        folder_structure <- paste(
            stringr::str_split(string = x, pattern = "/")[[1]][-c(seq(1,7))], 
            collapse = "/")
        destfile <- fix_path(file.path(save_dir, folder_structure))
        dir.create(dirname(destfile), showWarnings = FALSE, recursive = TRUE)
        if (!file.exists(destfile) | isTRUE(overwrite)) {
            message_parallel("Downloading file ==> ",destfile)
            download.file(url = x, destfile = destfile)
        }else {
            message_parallel("Importing previously downloaded file: ",destfile)
        }
        return(destfile)
    }, mc.cores = nThread)) 
    return(local_files)
}
