import_magma_files_sort <- function(meta,
                                    file_types = c(".genes.out",".genes.raw"),
                                    nested=FALSE,
                                    sorted=TRUE){
    id <- NULL;
    ids <- unique(meta$id)
    magma_files_list <- lapply(
        ids, 
        function(ID){
            mf <- c()
            if(".genes.out" %in% file_types) {
                mf["genes.out"] <- meta[id==ID,]$genes_out_url
            }
            if(".genes.raw" %in% file_types) {
                mf["genes.raw"] <- meta[id==ID,]$genes_raw_url
            } 
            return(mf)
        }) |> `names<-`(ids)
    if(isFALSE(nested)){
        magma_files <- unlist(magma_files_list)   
        if(isTRUE(sorted)){
            magma_files <- sort(magma_files)
        }
    }  
    return(magma_files)
}
