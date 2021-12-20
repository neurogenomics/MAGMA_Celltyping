magma_download_binary <- function(magma_url,
                                  dest_dir = find_install_dir(),
                                  verbose = TRUE) {
    #### Download the appropriate executable ####
    destfile <- file.path(dest_dir, basename(magma_url))
    destfile <- path.expand(destfile) 
    messager("Downloading MAGMA executable ==>",dest_dir, v = verbose)
    # Create the decompressed file name in advance
    destpath <- gsub("\\.zip$", "", destfile)
    options(timeout = 5 * 60)
    download.file(magma_url,
                  destfile = destfile
    )
    #### Adjust permissions to allow for folder manipulation ####
    try({system(paste0("chmod u=rx,go=rx ", destfile))})
    try({Sys.chmod(destfile, "777", use_umask = FALSE)})  
    #### Uzip folder ####
    try({
        utils::unzip(
            zipfile = destfile,
            junkpaths = TRUE,
            exdir = destpath,
            overwrite = TRUE
        )
    })
    # if(file.exists(destfile)){
    #     try({file.remove(destfile,
    #                      showWarnings = FALSE)})
    # }
    return(destpath)
}
