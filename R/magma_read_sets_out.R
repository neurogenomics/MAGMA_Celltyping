magma_read_sets_out <- function(out_prefix){
    #### MAGMA changed file formats ~1.09 ####
    version <- magma_installed_version(verbose = FALSE)
    v <- as.numeric(gsub(paste(letters,collapse = "|"),"",version))
    if(v < 1.09){
        path <- sprintf("%s.sets.out",out_prefix)
        res <- utils::read.table(file = path,
                                  header = TRUE,
                                  stringsAsFactors = FALSE) 
    } else {
        #### Not totally sure if this is right 
        path <- sprintf("%s.gsa.out", out_prefix)
        res <- utils::read.table(file = path, 
                                  header = TRUE,
                                  stringsAsFactors = FALSE) 
        res <- res[res$TYPE == "COVAR" & (res$MODEL == 1), ] 
    }
    return(res)
}
