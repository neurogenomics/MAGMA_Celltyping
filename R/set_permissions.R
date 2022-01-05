set_permissions <- function(path,
                            is_folder = FALSE,
                            verbose = TRUE){ 
    messager("Setting permissions for",
             paste0(if(is_folder)"folder"else"file","."),
             v=verbose)
    #### OS-specific commands ####
    if(get_os()=="Windows"){
        try({system(paste("icacls",
                          path,
                          if(is_folder) "/t" else NULL,
                          if(verbose) NULL else "/q",
                          "/grant:r Everyone:(OI)(CI)RX"))})
    } else {
        try({system(paste("chmod",
                          if(is_folder) "-R" else NULL,
                          "u=rx,go=rx",
                          path))})
    }
    #### Should work on all OS (maybe?) ####
    try({Sys.chmod(paths = path, 
                   mode = "777", 
                   use_umask = FALSE)})
}
