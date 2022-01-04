set_permissions <- function(path,
                            is_folder = FALSE,
                            verbose = TRUE){ 
    messager("Setting permissions for",
             paste0(if(is_folder)"folder"else"file","."),
             v=verbose)
    #### OS-specific commands ####
    if(get_os()=="Windows"){
        # https://docs.microsoft.com/en-us/previous-versions/windows/
        #   it-pro/windows-xp/bb490872(v=technet.10)?redirectedfrom=MSDN
        try({system(paste("cacls",
                          path,
                          if(is_folder) "\\t" else NULL,
                          "\\g rx"))})
    } else {
        try({system(paste("chmod",
                          if(is_folder) "-R" else NULL,
                          "u=rx,go=rx", path))})
    }
    #### Should work on all OS (maybe?) ####
    try({Sys.chmod(paths = path, 
                   mode = "777", 
                   use_umask = FALSE)})
}
