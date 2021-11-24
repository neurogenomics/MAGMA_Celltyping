test_that("drop_genes_within_mhc works", {
  
  if(!is_32bit()){
    gene_loc <- MAGMA.Celltyping:::get_genomeLocFile(build = "GRCH37")
    gene_loc2 <- paste0(gene_loc,"2")
    MAGMA.Celltyping::drop_genes_within_mhc(geneLocFilePath = gene_loc,
                                            geneLocFilePathOut = gene_loc2)
    
    dat1 <- data.table::fread(gene_loc)
    dat2 <- data.table::fread(gene_loc2)
    testthat::expect_lte(nrow(dat2), nrow(dat1))
  } 
})
