test_that("get_driver_genes works", {
  
    if(!is_32bit()){
        ctd <- ewceData::ctd()
        magma_dir <- MAGMA.Celltyping::import_magma_files()
        magma_res <- MAGMA.Celltyping::merge_results(
            MAGMA.Celltyping::enrichment_results)
        genesets <- MAGMA.Celltyping::get_driver_genes(ctd = ctd,
                                                       magma_res = magma_res,
                                                       GenesOut_dir = magma_dir,
                                                       fdr_thresh = 1)
        testthat::expect_true(methods::is(genesets,"list"))
        testthat::expect_true(methods::is(genesets[[1]]$level1,"list"))
    } else {
        testthat::expect_null(NULL)
    } 
})
