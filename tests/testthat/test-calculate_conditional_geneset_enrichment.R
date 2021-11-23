test_that("calculate_conditional_geneset_enrichment works", {
    
    ctd <- MAGMA.Celltyping::get_ctd("ctd_allKI")
    magma_dir <- MAGMA.Celltyping::import_magma_files(ids = "ieu-a-298")
    geneset <- MAGMA.Celltyping::rbfox_binding

    res <- MAGMA.Celltyping:: calculate_conditional_geneset_enrichment(
        geneset = geneset,
        ctd = ctd,
        controlledAnnotLevel = 1,
        controlledCTs = "pyramidal SS",
        magma_dir = magma_dir,
        analysis_name = "Rbfox_16_pyrSS",
        geneset_species = "mouse",
        ctd_species = "mouse")
    testthat::expect_true(methods::is(res,"data.frame"))
    cols <- c("NGENES","BASELINE_BETA","BASELINE_BETA_STD",
              "BASELINE_SE","BASELINE_P",
              "COND_BETA","COND_BETA_STD","COND_SE",
              "COND_P","conditionedCTs",
              "z","p_twoSided","p_oneSided")
    testthat::expect_true(all(cols %in% colnames(res)))
})
