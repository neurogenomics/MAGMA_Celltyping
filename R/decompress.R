decompress <- function(path_formatted,
                       remove=FALSE,
                       overwrite=FALSE){ 
    
    if(endsWith(path_formatted,".gz")){
        #### Decompress gzipped file #####
        destname <- file.path(tempdir(), gsub(".gz","",basename(path_formatted)))
        message("Saving decompressed copy of path_formatted ==> ",destname)
        if(!file.exists(destname) | overwrite){
            path_formatted2 <-  R.utils::gunzip(path_formatted, 
                                                destname=destname,
                                                remove=remove,
                                                overwrite=overwrite)
        }else{
            path_formatted2 <- destname 
        }
    } else if (endsWith(path_formatted,".bgz")){
        #### Decompress bgzipped file #####
        destname <- file.path(tempdir(), gsub(".bgz","",basename(path_formatted)))
        message("Saving decompressed copy of path_formatted ==> ",destname)
        if(!file.exists(destname) | overwrite){
            path_formatted2 <-  R.utils::bunzip2(path_formatted, 
                                                 destname=destname,
                                                 remove=remove,
                                                 overwrite=overwrite)
        }else{
            path_formatted2 <- destname 
        } 
    } else {
        path_formatted2 <- path_formatted 
    }
    return(as.character(path_formatted2))
}