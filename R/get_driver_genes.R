#' Get genes driving significant \pkg{MAGMA_celltyping} results
#' 
#' Infers the genes driving significant cell-type-specific enrichment results 
#' by computing the mean rank of the adjusted Z-score from the 
#' GWAS gene annotation file (\code{"ADJ_ZSTAT"}) and 
#' the cell-type specificity score from the CellTypeDataset 
#' (\code{"specificity_proportion"}). 
#' @param ctd CellTypeData object
#' @param ctd_species Either 'human' or 'mouse'
#' @param magma_res Merged results from \link[MAGMA.Celltyping]{merge_results}.
#' @param fdr_thresh FDR threshold for \code{magma_res}.
#' @param GenesOut_dir Folder to search for \emph{.genes.out} 
#' files implicated in \code{magma_res}.
#' @param n_genes Max number of drive genes to return per cell-type enrichment.
#' @param spec_deciles [Optional] 
#' Which \emph{"specificity_proportion"} deciles to 
#' include when calculating driver genes.
#' (10 = most specific).
#' @param verbose Print messages. 
#' @inheritParams calculate_celltype_associations
#' @inheritDotParams EWCE::standardise_ctd
#'
#' @examples
#' ctd <- ewceData::ctd()
#' GenesOut_dir <- MAGMA.Celltyping::import_magma_files()
#' magma_res <- MAGMA.Celltyping::merge_results(
#'     MAGMA.Celltyping::enrichment_results)
#' genesets <- MAGMA.Celltyping::get_driver_genes(ctd = ctd, 
#'                                                magma_res = magma_res, 
#'                                                GenesOut_dir = GenesOut_dir, 
#'                                                fdr_thresh = 1)
#' @export
#' @importFrom stringr str_split
#' @importFrom stats p.adjust setNames 
#' @importFrom EWCE standardise_ctd
get_driver_genes <- function(ctd,
                             ctd_species = infer_ctd_species(ctd = ctd),
                             prepare_ctd = TRUE,
                             magma_res, 
                             GenesOut_dir,
                             fdr_thresh = .05,
                             n_genes = 100,
                             spec_deciles = NULL,
                             verbose = TRUE,
                             ...) {
    # devoptera::args2vars(get_driver_genes)
    # scKirby::source_all()
    
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
    gwas_dict <- stats::setNames(
        stringr::str_split(basename(magma_GenesOut_files), "[.]")[[1]][1],
        magma_GenesOut_files
    )
    #### Prepare ctd ####
    if (isTRUE(prepare_ctd)) { 
        ctd <- EWCE::standardise_ctd(ctd = ctd,
                                     dataset = "NULL",
                                     input_species = ctd_species, 
                                     output_species = "human",
                                     verbose = verbose,
                                     ...)
        ctd_species <- "human"
    } 
    #### iterate over sig GWAS ####
    GENESETS <- lapply(magma_GenesOut_files, 
                       function(magma_GenesOut_file,
                                .ctd_species = ctd_species) {
        messager("+ Finding driver genes for:", 
                 gwas_dict[[magma_GenesOut_file]], "GWAS x CTD")
        magmaAdjZ <- adjust_zstat_in_genesOut(
            ctd = ctd, 
            prepare_ctd = FALSE,
            magma_GenesOut_file = magma_GenesOut_file,
            ctd_species = .ctd_species, 
            verbose = verbose
        )
        lapply(unique(sig_res$level), 
               function(annotLevel) {
            message("+ Level ", annotLevel)
            res <- calculate_celltype_enrichment_limma(
                magmaAdjZ = magmaAdjZ,
                ctd = ctd,
                prepare_ctd = FALSE,
                annotLevel = annotLevel,
                ctd_species = .ctd_species,
                celltypes = unique(sig_res$Celltype_id),
                return_all = TRUE,
                verbose = verbose
            )
            genesets <- create_genesets(
                res_input = res$input,
                n_genes = n_genes,
                spec_deciles = spec_deciles
            )
            return(genesets)
        }) |> `names<-`(paste0("level", unique(sig_res$level)))
    }) |> `names<-`(unname(gwas_dict))
    return(GENESETS)
}
