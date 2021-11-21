magma_download_binary <- function(magma_url,
                                  dest_dir = "/usr/local/bin") {
    #### Download the appropriate executable ####
    destfile <- file.path(dest_dir, basename(magma_url))
    # Create the decompressed file name in advance
    destpath <- gsub(".zip", "", destfile)
    options(timeout = 5 * 60)
    download.file(magma_url,
        destfile = destfile
    )
    utils::unzip(destfile,
        junkpaths = TRUE,
        exdir = gsub(".zip", "", destfile),
        overwrite = TRUE
    )
    if(file.exists(destfile)) file.remove(destfile, showWarnings = FALSE)
    return(destpath)
}
