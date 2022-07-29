import_magma_files_renest <- function(magma_files){
    if(methods::is(magma_files,"list")) {
        stopper("magma_files must be a character vector")
    }
    ids <- stringr::str_split(names(magma_files),"\\.", 
                              n = 2, 
                              simplify = TRUE)[,1]
    lapply(unique(ids), 
           function(ID){
               vec <- magma_files[ids==ID]
               names(vec) <- gsub(paste0(ID,"."),"",names(vec))
               return(vec)
           }) |> `names<-`(unique(ids)) 
}