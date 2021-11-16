check_access <- function(dest_dir){ 
    #### 0 == success, -1 === failure ####
    out <- file.access(dest_dir, mode = 2)
    have_write_access <- out==0
    return(have_write_access)
}