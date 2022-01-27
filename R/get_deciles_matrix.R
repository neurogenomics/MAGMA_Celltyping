#' Get deciles matrix
#' 
#' Extract the matrix of specificity deciles from a CellTypeDataset object.
#' Support function for
#'  \link[MAGMA.Celltyping]{create_top10percent_genesets_file}.
#' @inheritParams create_top10percent_genesets_file
#' 
#' @keywords internal
get_deciles_matrix <- function(ctd,
                             annotLevel,
                             ctd_species) {
    ctd2 <- map_specificity_to_entrez(
        ctd = ctd,
        annotLevel = annotLevel,
        ctd_species = ctd_species,
        return_ctd = TRUE, 
        use_matrix = "specificity_deciles"
    ) 
    quantDat2 <- ctd2[[annotLevel]]$quantDat2
    if (dim(quantDat2)[1] < 100) {
        stopper(
            "Less than one hundred genes detected after",
            "mapping genes between species.",
            "Was ctd_species defined correctly?"
        )
    }
    return(quantDat2)
}
