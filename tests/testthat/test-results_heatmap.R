test_that("results_heatmap works", {
    
    if(!is_32bit()){
        MAGMA_results <- MAGMA.Celltyping::enrichment_results
        merged_results <- MAGMA.Celltyping::merge_results(MAGMA_results)
        heat <- MAGMA.Celltyping::results_heatmap(
            merged_results = merged_results,
            fdr_thresh = 1)
        testthat::expect_true(methods::is(heat,"gg"))
    } else {
        testthat::expect_null(NULL)
    }
})
