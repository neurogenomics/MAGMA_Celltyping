check_entrez_genes <- function(geneset){
    n_valid <- sum(
        geneset %in% MAGMA.Celltyping::hgnc2entrez_orthogene$hgnc_symbol)
    if ((n_valid / length(geneset)) < 0.5) {
        stopper(
            "<50% of the geneset are recognised HGNC symbols",
            "with corresponding Entrez IDs.",
            "Check that ctd_species and geneset_species are set correctly."
        )
    }
}