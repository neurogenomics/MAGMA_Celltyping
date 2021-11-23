test_that("multiplication works", {
  
    
    gene_loc <- get_gene_loc(build = "GRCH37", overwrite = TRUE)
    gene_loc2 <- paste0(gene_loc,"2")
    MAGMA.Celltyping::drop_genes_within_mhc(geneLocFilePath = gene_loc,
                                            geneLocFilePathOut = gene_loc2)
    
    dat1 <- data.table::fread(gene_loc)
    dat2 <- data.table::fread(gene_loc2)
    testthat::expect_lt(nrow(dat2), nrow(dat1))
})
