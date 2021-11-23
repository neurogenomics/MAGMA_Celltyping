#' Check input to MAGMA cell type analyses
#'
#' This code was functonalised because it is called by both baseline
#' and conditional analysis.
#'
#' @inheritParams calculate_celltype_associations
#' @returns Nothing returned but sends error if there is a fault
#' with the arguments.
#' 
#' @keywords internal
check_inputs_to_magma_celltype_analysis <- function(ctd,
                                                    gwas_sumstats_path, 
                                                    upstream_kb,
                                                    downstream_kb) {
    sumstatsPrefix <- sprintf(
        "%s.%sUP.%sDOWN",
        gwas_sumstats_path, upstream_kb, downstream_kb
    )
    magmaPaths <- get_magma_paths(
        gwas_sumstats_path = gwas_sumstats_path,
        upstream_kb = upstream_kb,
        downstream_kb = downstream_kb
    )
    #### Error checks ####
    # - Does ctd have quantiles?
    for (annotLevel in seq_len(length(ctd))) {
        if (!"specificity_quantiles" %in% names(ctd[[annotLevel]])) {
            stopper(
                "CTD should have quantiles.",
                "Send to 'prepare_quantile_groups'",
                "before calling this function."
            )
        }
    }
    # - Is annotLevel within length(ctd)
    if (!annotLevel %in% seq_len(length(ctd))) {
        stopper(
            "annotLevel does not correspond to",
            "a level available with ctd[[annotLevel]]"
        )
    }
    # - Does genome_ref_path.bed exist?
    # if (!file.exists(sprintf("%s.bed", genome_ref_path))) {
    #     stop(sprintf("%s.bed does not exist", genome_ref_path))
    # }
    # - Check the genes.raw file exists
    if (!file.exists(sprintf("%s.genes.out", magmaPaths$filePathPrefix))) {
        stopper(
            paste0(magmaPaths$filePathPrefix, ".genes.out"),
            "does not exist. Run map_snps_to_genes() before this function"
        )
    }
}
