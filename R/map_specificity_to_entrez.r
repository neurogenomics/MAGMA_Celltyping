#' Map specificity to Entrez gene IDs
#'
#' Convenience function used in \link[MAGMA.Celltyping]{create_gene_covar_file}.
#'
#' @param return_ctd Return the actual CellTypeDataset,
#'  rather than path to where it is saved.
#' @param use_saved Use a saved version of the gene mapping table.
#' @inheritParams calculate_celltype_associations
#'
#' @returns Matrix in which the first column is 'entrez'
#' and then the specificity decile for each cell type
#'
#' @keywords internal
#' @importFrom dplyr %>% rename
map_specificity_to_entrez <- function(ctd,
                                      annotLevel,
                                      ctd_species,
                                      return_ctd = FALSE,
                                      use_saved = TRUE,
                                      verbose = TRUE) {
    # Because sumstats use entrez genes & ctd uses
    # gene symbols, match entrez-->symbols
    messager("Mapping gene symbols in specificity quantiles matrix",
             "to entrez IDs.",v=verbose)
    #### Extract gene list ####
    mat <- ctd[[annotLevel]]$specificity_quantiles 
    #### Find 1:1 orthologs ####
    # Skip this step if the genes are already human
    if (ctd_species != "human") {
        output_species <- "human"
        mat <- orthogene::convert_orthologs(
            gene_df = mat,
            gene_input = "rownames",
            gene_output = "rownames",
            input_species = ctd_species,
            output_species = output_species,
            method = "homologene",
            verbose = verbose
        )
        ctd_species <- output_species
    }
    #### Map gene symbols to Entrez IDs ####
    # Queries the respective species 
    if(use_saved){
        gene_map <- MAGMA.Celltyping::hgnc2entrez_orthogene
    } else {
        gene_map <- orthogene::map_genes(
            genes = genes,
            species = ctd_species,
            target = "ENTREZGENE_ACC",
            drop_na = TRUE,
            # only return 1 Entrez ID per gene symbol
            mthreshold = 1,
            verbose = verbose
        )
        gene_map <- gene_map %>% dplyr::rename(hgnc_symbol = input, 
                                               entrez = target)
    }
    #### Subset gene_map #### 
    gene_map <- gene_map[gene_map$hgnc_symbol %in% rownames(mat),]
    gene_map <- gene_map[!duplicated(gene_map$hgnc_symbol),]
    #### Subset matrix ####
    quantDat2 <- data.frame(
        entrez = gene_map$entrez,
        as.matrix(mat[gene_map$hgnc_symbol, ]),
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
