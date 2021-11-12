#' Merge MAGMA results
#'
#' Used for instance when you want to merge results from 'Linear' and 'Top 10%' enrichment modes
#'
#' @param ctAssoc1 Output from either calculate_celltype_associations() or calculate_conditional_celltype_associations()
#' @param ctAssoc2 Output from either calculate_celltype_associations() or calculate_conditional_celltype_associations()
#'
#' @return List with same format as output from either calculate_celltype_associations() or calculate_conditional_celltype_associations()
#'
#' @examples
#' ctAssocs <- merge_magma_results.r(ctAssoc1, ctAssoc2)
#' @export
merge_magma_results <- function(ctAssoc1, ctAssoc2) {
    ctAssoc <- list()

    for (annLev in 1:sum(names(ctAssoc1) == "")) {
        ctAssoc[[annLev]] <- list()
        ctAssoc[[annLev]]$results <- rbind(ctAssoc1[[annLev]]$results, ctAssoc2[[annLev]]$results)
    }

    if (ctAssoc1$gwas_sumstats_path == ctAssoc2$gwas_sumstats_path) {
        ctAssoc$gwas_sumstats_path <- ctAssoc2$gwas_sumstats_path
    }
    if (ctAssoc1$total_baseline_tests_performed == ctAssoc2$total_baseline_tests_performed) {
        ctAssoc$total_baseline_tests_performed <- ctAssoc2$total_baseline_tests_performed
    }
    if (ctAssoc1$upstream_kb == ctAssoc2$upstream_kb) {
        ctAssoc$upstream_kb <- ctAssoc2$upstream_kb
    }
    if (ctAssoc1$downstream_kb == ctAssoc2$downstream_kb) {
        ctAssoc$downstream_kb <- ctAssoc2$downstream_kb
    }
    if (ctAssoc1$genome_ref_path == ctAssoc2$genome_ref_path) {
        ctAssoc$genome_ref_path <- ctAssoc2$genome_ref_path
    }

    return(ctAssoc)
}
