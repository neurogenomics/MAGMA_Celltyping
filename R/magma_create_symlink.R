magma_create_symlink <- function(dest_magma,
                                 upgrade,
                                 verbose = TRUE){
    
    messager("Creating symlink so MAGMA can be executed",
             "via the command line: 'magma <arguments>'",
             v = verbose
    )
    dest_dir <- dirname(dirname(dest_magma))
    ## If the exec isn't in bin, need to set up symlink  
    if(basename(dest_dir)!="bin" | 
       (!magma_installed(verbose = FALSE))){
        symlink <- R.utils::createLink(
            link = "magma",
            target = dest_magma,
            overwrite = upgrade
        )
    } else {
        #### Copy into main bin dir ####
        ## There, the system should automatically create a symlink for it 
        file.copy(from = dest_magma,
                  to = dirname(dirname(dest_magma)), 
                  overwrite = upgrade) 
        symlink <- "magma"
    }
    return(symlink)
}