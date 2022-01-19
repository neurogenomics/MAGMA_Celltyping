test_that("calculate_celltype_enrichment_limma works", {
  
    if(!is_32bit()){ 
        ctd <- ewceData::ctd()
        # The package stores an example genesOut file, so save this to a tempfile
        myGenesOut <- MAGMA.Celltyping::import_magma_files(
            ids = c("ieu-a-298"),
            file_types = ".genes.out",
            return_dir = FALSE)

        #### Get adjust gene z-scores ####
        magmaAdjZ <- MAGMA.Celltyping::adjust_zstat_in_genesOut(
            ctd = ctd,
            magma_GenesOut_file = myGenesOut)
        testthat::expect_true(methods::is(magmaAdjZ,"data.frame"))

        #### Get cell type enrichment ####
        ps <- MAGMA.Celltyping::calculate_celltype_enrichment_limma(
            magmaAdjZ = magmaAdjZ,
            annotLevel = 1,
            ctd = ctd)
        celltypes <- EWCE::fix_celltype_names(colnames(ctd[[1]]$mean_exp))
        testthat::expect_true(methods::is(ps,"numeric"))
        testthat::expect_equal(names(ps), celltypes)
    } else {
        testthat::expect_null(NULL)
    }
})
