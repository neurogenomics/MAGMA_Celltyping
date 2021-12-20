#' Find a directory to install software in
#' 
#' Find a directory that you have write permissions in to so that you can 
#' install software there. 
#' Tests several options and returns the first viable one.
#'
#' @param dest_dir_opts Potential installation directory options.
#' @param verbose Print messages.
#' 
#' @return Path to viable installation directory.
#' @keywords internal
find_install_dir <- function(dest_dir_opts = c(
    tools::R_user_dir("MAGMA.Celltyping", which="cache"),
    system.file("tools",package = "MAGMA.Celltyping"),
    "/usr/local/bin",
    Sys.getenv("HOME"),
    getwd(),
    tempdir()),
                             verbose = FALSE){ 
    dir.create(tools::R_user_dir("MAGMA.Celltyping", which="cache"),
               showWarnings = FALSE)
    # messager("Searching for viable installation directory.",v=verbose)
    have_access <- check_access(dest_dir = dest_dir_opts)
    use_dir <- names(have_access[have_access][1])
    if(length(use_dir)>0){
        messager("Found viable installation directory:",use_dir,
                 v=verbose)    
    } else {
        stop("No viable installation directory identified.")
    }
    use_dir <- path.expand(use_dir)
    return(use_dir)
}
