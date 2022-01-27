test_that("merge_results works", {
    
    if(!is_32bit()){
        #### Use precomputed results: see ?enrichment_results for details  ####
        MAGMA_results <- MAGMA.Celltyping::enrichment_results
        #### Merge results ####
        merged_res <- MAGMA.Celltyping::merge_results(MAGMA_results)
        ## Test
        testthat::expect_true(methods::is(merged_res,"data.frame"))
        key_cols <- c("GWAS","Celltype","OBS_GENES","BETA","BETA_STD","SE")
        testthat::expect_true(all(key_cols %in% colnames(merged_res)))
    } else {
        testthat::expect_null(NULL)
    }
})
