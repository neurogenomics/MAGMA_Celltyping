test_that("merge_results works", {
    
    #### Use precomputed results: see ?enrichment_results for details  ####
    MAGMA_results <- MAGMA.Celltyping::enrichment_results

    #### Merge results ####
    merged_res <- MAGMA.Celltyping::merge_results(MAGMA_results)
    
    testthat::expect_true(methods::is(merged_res,"data.frame"))
    key_cols <- c("GWAS","Celltype","OBS_GENES","BETA","BETA_STD","SE")
    testthat::expect_true(all(key_cols %in% colnames(merged_res)))
})
