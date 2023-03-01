#' Merge MAGMA results
#'
#' Used for instance when you want to merge results from 
#' 'Linear' and 'Top 10\%' enrichment modes.
#'
#' @param ctAssoc1 Output from either 
#' \link[MAGMA.Celltyping]{calculate_celltype_associations} or 
#' \link[MAGMA.Celltyping]{calculate_conditional_celltype_associations}.
#' @param ctAssoc2 Output from either 
#' \link[MAGMA.Celltyping]{calculate_celltype_associations} or 
#' \link[MAGMA.Celltyping]{calculate_conditional_celltype_associations}.
#'
#' @returns List with same format as output from either 
#' \link[MAGMA.Celltyping]{calculate_celltype_associations} or
#' \link[MAGMA.Celltyping]{calculate_conditional_celltype_associations}.
#'
#' @export
#' @importFrom stats setNames
#' @examples
#' res <- MAGMA.Celltyping::enrichment_results
#' ctAssoc1 <- res[[1]]$ctAssocsLinear
#' ctAssoc2 <- res[[1]]$ctAssocsTop
#' ctAssocs <- merge_magma_results(ctAssoc1=ctAssoc1, ctAssoc2=ctAssoc2)
merge_magma_results <- function(ctAssoc1,
                                ctAssoc2) {
    
    #### Find the number of levels ####
    levels_count <- sum(
        names(ctAssoc1) == "" | startsWith(names(ctAssoc1),"level") 
    )
    #### Merge multiple association results ####
    ctAssoc <- lapply(stats::setNames(seq_len(levels_count),
                           paste0("level",seq_len(levels_count))), 
                      function(annLev){
                   list(results=rbind(ctAssoc1[[annLev]]$results,
                                      ctAssoc2[[annLev]]$results))         
    }) 
    #### Add metadata to merged ctAssoc object ####
    if (ctAssoc1$gwas_sumstats_path == ctAssoc2$gwas_sumstats_path) {
        ctAssoc$gwas_sumstats_path <- ctAssoc2$gwas_sumstats_path
    }
    if (ctAssoc1$total_baseline_tests_performed == 
        ctAssoc2$total_baseline_tests_performed) {
        ctAssoc$total_baseline_tests_performed <- 
            ctAssoc2$total_baseline_tests_performed
    }
    if (ctAssoc1$upstream_kb == ctAssoc2$upstream_kb) {
        ctAssoc$upstream_kb <- ctAssoc2$upstream_kb
    }
    if (ctAssoc1$downstream_kb == ctAssoc2$downstream_kb) {
        ctAssoc$downstream_kb <- ctAssoc2$downstream_kb
    } 
    return(ctAssoc)
}
