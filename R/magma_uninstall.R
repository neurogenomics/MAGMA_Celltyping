#' Uninstall MAGMA
#' 
#' Uninstall one or all versions of MAGMA.
#' 
#' @param version Which version of MAGMA to uninstall. If \code{NULL}, 
#' will uninstall all versions found.
#' @param verbose Print messages.
#' 
#' @return Path(s) to MAGMA installation folder(s) that were deleted.
#' @export
#' @examples 
#' MAGMA.Celltyping::magma_uninstall()
magma_uninstall <- function(version = NULL,
                            verbose = TRUE){
    magma_x = magma_executable(version = version,
                               return_all = is.null(version),
                               verbose = verbose)
    if(length(magma_x)>0){
        messager("Removing",length(magma_x),"installed versions of MAGMA.",
                 v=verbose)
        out1 <- unlink(dirname(magma_x), 
                       recursive = TRUE, 
                       force = TRUE)
        out2 <- file.remove(paste0(dirname(magma_x),".zip"))
    } else {
        messager("There are no MAGMA installations to remove.",
                 v=verbose)
    }
    return(dirname(magma_x))
}
