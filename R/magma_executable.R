#' Find the local executable
#' 
#' Find the local executable for a specific OS.
#' @source \href{https://ctg.cncr.nl/software/MAGMA/prog/archive/}{
#' MAGMA archive}
#' 
#' @keywords internal
#' @return Path to appropriate executable
#' @param verbose Print messages. 
magma_executable <- function(exec_dir = find_install_dir(verbose=FALSE),
                             check_exists = FALSE,
                             version = NULL,
                             return_all = FALSE,
                             verbose = FALSE){
    #### Find the executable file once it's been downloaded ####
    magma_x <- magma_find_executable(destpath = exec_dir,
                                     return_all = return_all) 
    magma_x <- magma_x[!is.na(magma_x)]
    magma_x <- fix_path(magma_x) 
    if(!is.null(version)){
        v_search <- grep(version,magma_x, value = TRUE, ignore.case = TRUE)
        v_search <- v_search[!is.na(v_search)]
        if(length(v_search)>0) {
            magma_x <- v_search
        } else {
            messager("No MAGMA versions matching",paste0("'",version,"'"),
                     "are currently installed.", v=verbose)
        }
    } 
    magma_x <- magma_executable_select(magma_x = magma_x, 
                                       return_all = return_all, 
                                       verbose = verbose)
    if(check_exists && !file.exists(magma_x)) {
        stop_msg <- paste("Cannot find MAGMA executable.")
        stop(stop_msg)
    } 
    return(magma_x)
}
