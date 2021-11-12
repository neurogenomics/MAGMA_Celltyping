github_download_files <- function(filelist,
                                  download_dir = tempdir(),
                                  overwrite = F,
                                  nThread = parallel::detectCores() - 2,
                                  verbose = T) {
    printer("+ Downloading", length(filelist), "files...", v = verbose)
    local_files <- unlist(parallel::mclapply(filelist, function(x) {
        print(paste("Downloading", x))
        branch <- stringr::str_split(string = x, pattern = "/")[[1]][7]
        folder_structure <- paste(stringr::str_split(string = x, pattern = "/")[[1]][-c(1:7)], collapse = "/")
        destfile <- file.path(download_dir, folder_structure)
        dir.create(dirname(destfile), showWarnings = F, recursive = T)
        if (!file.exists(destfile) & overwrite == F) download.file(url = x, destfile = destfile)
        return(destfile)
    }, mc.cores = nThread))
    return(local_files)
}
