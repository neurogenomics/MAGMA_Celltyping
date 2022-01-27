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
#' @param verbose Print messages. 
#'
#' @examples
#' ctd <- ewceData::ctd()
#' magma_dir <- MAGMA.Celltyping::import_magma_files()
#' magma_res <- MAGMA.Celltyping::merge_results(
#'     MAGMA.Celltyping::enrichment_results)
#' genesets <- MAGMA.Celltyping::get_driver_genes(ctd = ctd, 
#'                                                magma_res = magma_res, 
#'                                                GenesOut_dir = magma_dir, 
#'                                                fdr_thresh = 1)
#' @export
#' @importFrom stringr str_split
#' @importFrom stats p.adjust
#' @importFrom dplyr %>%
get_driver_genes <- function(ctd,
                             ctd_species = "mouse",
                             magma_res,
                             GenesOut_dir,
                             fdr_thresh = .05,
                             n_genes = 100,
                             spec_deciles = 10,
                             verbose = TRUE) {
    #### Avoid confusing checks ####
    FDR <- NULL;
    #### Find .genes.out files for sig GWAS ####
    messager("Filtering @ FDR<", fdr_thresh, v=verbose)
    if(!"FDR" %in% colnames(magma_res)) {
        magma_res$FDR <- stats::p.adjust(magma_res$P, method="fdr")
    }
    sig_res <- subset(magma_res, FDR < fdr_thresh)
    if(nrow(sig_res)==0) stop("No significant results detected.")
    magma_GenesOut_files <- find_GenesOut_files(GenesOut_dir = GenesOut_dir,
                                                verbose = verbose) 
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
                celltypes = sig_res$Celltype,
                return_all = TRUE
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
