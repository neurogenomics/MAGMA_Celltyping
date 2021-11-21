test_that("results_heatmap works", {
    
    MAGMA_results <- MAGMA.Celltyping::enrichment_results
    merged_results <- MAGMA.Celltyping::merge_results(res)
    heat <- MAGMA.Celltyping::results_heatmap(
        merged_results = merged_results,
        fdr_thresh = 1)
    testthat::expect_true(methods::is(heat,"gg"))
})
