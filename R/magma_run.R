#' Run MAGMA
#' 
#' Find the local MAGMA executable for a specific OS and run a command with it.
#' 
#' @param version MAGMA version to use.
#' @param cmd Commands to MAGMA as a string or list of strings.
#' @param verbose Print messages.
#' @source \href{https://ctg.cncr.nl/software/MAGMA/prog/archive/}{
#' MAGMA archive}
#' @export
#' @examples 
#' MAGMA.Celltyping::magma_run(cmd = "--version")
magma_run <- function(version = NULL,
                      cmd = NULL,
                      verbose = FALSE){
    magma_x = magma_executable(version = version,
                               verbose = verbose)
    #### Collapse vector/list into one string ####
    if(length(cmd)>1){
        cmd <- paste(cmd, collapse = " ")
    }
    cmd2 <- paste(magma_x,
                  gsub("^magma|^[ ]","",cmd))
    #### Print the command about to be run #### 
    if(verbose) {
        magma_path <- strsplit(cmd2," ")[[1]][1]
        msg <- gsub(paste0("^",magma_path),"magma", cmd2)
        message_cmd(msg)
    }
    #### Run the command ####
    out <- system(cmd2)
}
