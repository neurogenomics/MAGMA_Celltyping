github_download_files <- function(filelist,
                                  save_dir = tempdir(),
                                  overwrite = FALSE,
                                  nThread = 1,
                                  verbose = TRUE) {
    requireNamespace("stringr")
    requireNamespace("parallel")
    
    messager("Downloading", length(filelist),"files.", v = verbose)
    local_files <- unlist(parallel::mclapply(filelist, function(x) { 
        branch <- stringr::str_split(string = x, pattern = "/")[[1]][7]
        folder_structure <- paste(
            stringr::str_split(string = x, pattern = "/")[[1]][-c(seq(1,7))], 
            collapse = "/")
        destfile <- file.path(save_dir, folder_structure)
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
