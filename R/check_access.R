#' Check access
#' 
#' Check if one has access to directory
#' @param dest_dir Directory to check. 
#' @keywords internal
check_access <- function(dest_dir){ 
    #### 0 == success, -1 === failure ####
    out <- file.access(dest_dir, mode = 2)
    have_write_access <- out==0
    return(have_write_access)
}