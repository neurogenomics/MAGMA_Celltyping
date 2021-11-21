magma_read_gsa_out <- function(out_prefix,
                               analysis_name){
    path <- sprintf("%s.gsa.out", out_prefix)
    res_cond <- read.table(path, 
                           header = TRUE,
                           stringsAsFactors = FALSE) 
    res_cond <- res_cond[res_cond$VARIABLE == analysis_name, ]
    return(res_cond)
}
