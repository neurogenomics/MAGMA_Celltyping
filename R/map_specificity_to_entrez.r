#' Map specificity to Entrez gene IDs
#'
#' Convenience function used in \link[MAGMA.Celltyping]{create_gene_covar_file}.
#'
#' @param return_ctd Return the actual CellTypeDataset,
#'  rather than path to where it is saved.
#' @inheritParams calculate_celltype_associations
#'
#' @returns Matrix in which the first column is 'entrez'
#' and then the specificity decile for each cell type
#'
#' @keywords internal
map_specificity_to_entrez <- function(ctd,
                                      annotLevel,
                                      sctSpecies,
                                      return_ctd = FALSE,
                                      verbose = TRUE) {
    # Because sumstats use entrez genes & ctd uses
    # gene symbols, match entrez-->symbols

    #### Extract gene list ####
    mat <- ctd[[annotLevel]]$specificity_quantiles
    genes <- rownames(mat)
    #### Find 1:1 orthologs ####
    # Skip this step if the genes are already human
    if (sctSpecies != "human") {
        orths <- orthogene::convert_orthologs(
            gene_df = genes,
            gene_output = "columns",
            input_species = sctSpecies,
            output_species = "human",
            method = "homologene",
            verbose = verbose
        )
        genes <- orths$input_gene
    }
    #### Map gene symbols to Entrez IDs ####
    # Queries the respective species
    gene_map <- orthogene::map_genes(
        genes = genes,
        species = sctSpecies,
        numeric_ns = "ENTREZGENE",
        drop_na = TRUE,
        verbose = verbose
    )
    #### Subset matrix ####
    quantDat <- as.matrix(mat[gene_map$input, ])
    quantDat2 <- data.frame(
        entrez = gene_map$target,
        quantDat,
        check.rows = FALSE,
        check.names = FALSE
    )
    quantDat2 <- quantDat2[!duplicated(quantDat2$entrez), ]
    #### Return resusults ####
    if (return_ctd) {
        ctd[[annotLevel]]$quantDat2 <- quantDat2
        return(ctd)
    } else {
        return(quantDat2)
    }
}
