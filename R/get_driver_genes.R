#' Get genes driving significant \pkg{MAGMA_celltyping} results
#'
#' @param ctd CellTypeData object
#' @param ctd_species Either 'human' or 'mouse'
#' @param magma_res Merged results from \code{MAGMA.Celltyping::merge_results}.
#' @param fdr_thresh FDR threshold for \code{magma_res}.
#' @param GenesOut_dir Folder to search for \emph{.genes.out} 
#' files implicated in \code{magma_res}.
#' @param n_genes Max number of drive genes to return per cell-type enrichment.
#' @param spec_deciles Which \emph{specificity_proportion} deciles to 
#' include when calculating driver genes.
#' (10 = most specific).
#'
#' @export
get_driver_genes <- function(ctd,
                             ctd_species = "mouse",
                             magma_res,
                             fdr_thresh = .05,
                             GenesOut_dir = "./",
                             n_genes = 100,
                             spec_deciles = 10) {
    #### Avoid confusing checks ####
    FDR <- NULL;
    #### Find .genes.out files for sig GWAS ####
    message("Filtering @ FDR<", fdr_thresh)
    sig_res <- subset(magma_res, FDR < fdr_thresh)
    magma_GenesOut_files <- find_GenesOut_files(
        sig_res = sig_res,
        root_dir = GenesOut_dir
    )
    gwas_dict <- setNames(
        stringr::str_split(basename(magma_GenesOut_files), "[.]")[[1]][1],
        magma_GenesOut_files
    )
    #### iterate over sig GWAS ####
    GENESETS <- lapply(magma_GenesOut_files, function(genesout,
                                                      .ctd_species = ctd_species) {
        message("+ Finding driver genes for: ", gwas_dict[[genesout]], " GWAS x CTD")
        magmaAdjZ <- adjust_zstat_in_genesOut(
            ctd = ctd,
            magma_GenesOut_file = genesout,
            ctd_species = .ctd_species
        )
        lapply(unique(sig_res$level), function(annotLevel) {
            message("+ Level ", annotLevel)
            res <- calculate_celltype_enrichment_limma(
                magmaAdjZ = magmaAdjZ,
                ctd = ctd,
                annotLevel = annotLevel,
                ctd_species = .ctd_species,
                celltypes = sig_res$Celltype_id,
                return_all = T
            )
            genesets <- create_genesets(
                res_input = res$input,
                n_genes = n_genes,
                spec_deciles = spec_deciles
            )
            return(genesets)
        }) %>% `names<-`(paste0("level", unique(sig_res$level)))
    }) %>% `names<-`(unname(gwas_dict))
    return(GENESETS)
}
