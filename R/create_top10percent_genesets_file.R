#' Create gene covar file
#'
#' The gene covar file is the input to MAGMA for the celltype
#' association analysis. This code was functonalised
#' because it is called by both baseline and conditional analysis.
#'
#' @param genesOutFile The output of the second call to MAGMA
#'  (performed in the map_snps_to_genes function).
#' @inheritParams calculate_celltype_associations
#'
#' @return Filepath for the gene covar file
#'
#' @source 
#' \code{
#' #### Prepare cell-type data ####
#' ctd <- ewceData::ctd()
#' 
#' #### Prepare GWAS MAGMA data ####
#' myGenesOut <- MAGMA.Celltyping::import_magma_files(ids = "ieu-a-298",
#'                                                   file_types = ".genes.out",
#'                                                   return_dir = FALSE)
#' ctd <- MAGMA.Celltyping::prepare_quantile_groups(ctd = ctd)
#' geneSetsFilePath <- MAGMA.Celltyping:::create_top10percent_genesets_file(
#'     genesOutFile = myGenesOut,
#'     ctd = ctd,
#'     annotLevel = 1,
#'     ## Mapped to human orths by prepare_quantile_groups previously
#'     ctd_species = "human"
#' )
#' } 
#' @keywords internal
#' @importFrom dplyr setdiff
#' @importFrom EWCE fix_celltype_names
create_top10percent_genesets_file <- function(genesOutFile,
                                              ctd,
                                              annotLevel,
                                              ctd_species,
                                              verbose = TRUE) {
    #### Map genes first so that the deciles computed
    # in the following step only include usable genes ####
    quantDat2 <- get_deciles_matrix(
        ctd = ctd,
        annotLevel = annotLevel,
        ctd_species = ctd_species
    )
    ## Check column names don't have spaces and other 
    ## problematic characters.
    colnames(quantDat2) <- EWCE::fix_celltype_names(
        celltypes = colnames(quantDat2))
    #### Construct top10% specificity gene markers for each cell type ####
    messager("Constructing top10% gene marker sets for",
        ncol(quantDat2), "cell-types.",
        v = verbose
    )
    cts <- dplyr::setdiff(colnames(quantDat2), "entrez")
    ctRows <- rep("", length(cts))
    names(ctRows) <- cts
    for (ct in cts) {
        ctRows[ct] <- paste(c(ct, quantDat2[quantDat2[, ct] == 10, "entrez"]),
            collapse = " "
        )
    }
    #### Write genes covar file to disk ####
    geneCovarFile <- tempfile()
    write.table(
        x = ctRows,
        file = geneCovarFile,
        quote = FALSE,
        row.names = FALSE,
        sep = "\t",
        col.names = FALSE
    )
    return(geneCovarFile)
}
