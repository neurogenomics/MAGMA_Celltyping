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
#' @examples
#' myGenesOut <- tempfile()
#' ctd <- ewceData::ctd()
#' data.table::fwrite(
#'     x = MAGMA.Celltyping::genesOut,
#'     file = myGenesOut,
#'     sep = "\t"
#' )
#' ctd <- MAGMA.Celltyping::prepare_quantile_groups(ctd = ctd)
#' geneSetsFilePath <- MAGMA.Celltyping::create_top10percent_genesets_file(
#'     genesOutFile = myGenesOut,
#'     ctd = ctd,
#'     annotLevel = 1,
#'     sctSpecies = "mouse"
#' )
#' @keywords internal
#' @importFrom dplyr setdiff
create_top10percent_genesets_file <- function(genesOutFile,
                                              ctd,
                                              annotLevel,
                                              sctSpecies,
                                              verbose = TRUE) {
    #### Map genes first so that the deciles computed
    # in the following step only include usable genes ####
    quantDat2 <- get_top10percent(
        ctd = ctd,
        annotLevel = annotLevel,
        sctSpecies = sctSpecies
    )
    #### Check column names don't have spaces ####
    colnames(quantDat2) <- check_celltype_names(ct_names = colnames(quantDat2))
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
