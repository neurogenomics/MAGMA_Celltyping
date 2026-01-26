test_that("result_barplot works", {
    MAGMA_results <- MAGMA.Celltyping::enrichment_results
    merged_results <- MAGMA.Celltyping::merge_results(MAGMA_results)
    
    bar <- MAGMA.Celltyping::results_barplot(
             merged_results = merged_results,
             fdr_thresh = 1)  
    
    expect_s3_class(bar, "ggplot")
})
