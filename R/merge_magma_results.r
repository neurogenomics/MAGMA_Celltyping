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
#' @return List with same format as output from either 
#' \link[MAGMA.Celltyping]{calculate_celltype_associations} or
#' \link[MAGMA.Celltyping]{calculate_conditional_celltype_associations}.
#'
#' @examples
#' \dontrun{
#' ctAssocs <- MAGMA.Celltyping::merge_magma_results(ctAssoc1, ctAssoc2)
#' }
#' @export
merge_magma_results <- function(ctAssoc1,
                                ctAssoc2) {
    ctAssoc <- list()
    for (annLev in seq(1,sum(names(ctAssoc1) == ""))) {
        ctAssoc[[annLev]] <- list()
        ctAssoc[[annLev]]$results <- rbind(ctAssoc1[[annLev]]$results,
                                           ctAssoc2[[annLev]]$results)
    }

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
