test_that("calculate_conditional_celltype_associations works", {
  
    #### Prepare cell-type data ####
    ctd <- ewceData::ctd()
    #### Prepare GWAS MAGMA data ####
    magma_dir <- MAGMA.Celltyping::import_magma_files(ids = "ieu-a-298")

    #### Run pipeline ####
    ctAssocs <- MAGMA.Celltyping::calculate_conditional_celltype_associations(
        ctd = ctd,
        controlledAnnotLevel = 1,
        controlTopNcells = 1,
        magma_dir = magma_dir,
        ctd_species = "mouse")
    testthat::expect_null(ctAssocs)
    
    
    #### Raise the qvalue threshold ####
    ctAssocs2 <- MAGMA.Celltyping::calculate_conditional_celltype_associations(
        ctd = ctd,
        controlledAnnotLevel = 1,
        controlTopNcells = 1,
        qvalue_thresh = 1,
        magma_dir = magma_dir,
        ctd_species = "mouse")
    lvl <- 1
    ctd_celltypes <- EWCE::fix_celltype_names(colnames(ctd[[lvl]]$mean_exp))
    testthat::expect_true(all(ctd_celltypes %in% ctAssocs2$level1$results$Celltype))
})
