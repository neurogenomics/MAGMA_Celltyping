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
})
