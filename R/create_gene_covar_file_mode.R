#' Create gene covariance file: by mode
#' 
#' Create gene covariance file for a given \code{EnrichmentMode}.
#' 
#' @param magmaPaths Output of \code{get_magma_paths}. 
#' @inheritParams calculate_conditional_celltype_associations
#' 
#' @keywords internal
create_gene_covar_file_mode <- function(EnrichmentMode,
                                        magmaPaths,
                                        ctd, 
                                        annotLevel, 
                                        ctd_species){
    if (EnrichmentMode == "Linear") {
        genesCovarFile <- create_gene_covar_file(
            genesOutFile = sprintf(
                "%s.genes.out", magmaPaths$filePathPrefix
            ),
            ctd = ctd,
            annotLevel = annotLevel,
            ctd_species = ctd_species
        )
    } else {
        genesCovarFile <- create_top10percent_genesets_file(
            genesOutFile = sprintf(
                "%s.genes.out", magmaPaths$filePathPrefix
            ),
            ctd = ctd,
            annotLevel = annotLevel,
            ctd_species = ctd_species
        )
    }
    return(genesCovarFile)
}
