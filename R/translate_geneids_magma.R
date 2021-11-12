translate_geneids_magma <- function(magma_dt,
                                    gene_col = "GENE") {
    all_hgnc_wtEntrez <- MAGMA.Celltyping::all_hgnc_wtEntrez
    gene_key <- data.table::data.table(all_hgnc_wtEntrez,
        key = "entrezgene"
    )
    magma_dt[, GENE := gene_key[get(gene_col), "hgnc_symbol"]]
    magma_dt <- subset(magma_dt, !is.na(GENE))
    return(magma_dt)
}
