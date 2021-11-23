test_that("calculate_geneset_enrichment works", {
    
    magma_dir <- MAGMA.Celltyping::import_magma_files(ids = "ieu-a-298")
    geneset <- MAGMA.Celltyping::rbfox_binding

    res <- MAGMA.Celltyping::calculate_geneset_enrichment(
        geneset = geneset, 
        magma_dir = magma_dir,
        analysis_name = "Rbfox_16_pyrSS",
        geneset_species = "mouse")
    testthat::expect_true(methods::is(res,"data.frame"))
    cols <- c("VARIABLE","TYPE","NGENES","BETA","BETA_STD","SE","P")
    testthat::expect_true(all(cols %in% colnames(res)))
})
