#' Set permissions
#' 
#' Robust methods for setting file/folder permissions across multiple OS.
#' 
#' @param path Path to file/folder.
#' @param is_folder Whether \code{path} is a folder. 
#' If so, permissions will be set recursively for all files/subfolders. 
#' @param verbose Print messages. 
#' 
#' @keywords internal
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
