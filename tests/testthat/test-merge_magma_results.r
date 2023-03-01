test_that("merge_magma_results works", {
  
    res <- MAGMA.Celltyping::enrichment_results
    ctAssoc1 <- res[[1]]$ctAssocsLinear
    ctAssoc2 <- res[[1]]$ctAssocsTop
    ctAssocs <- merge_magma_results(ctAssoc1=ctAssoc1, ctAssoc2=ctAssoc2)
    testthat::expect_equal(
        nrow(ctAssocs[[1]]$results),
        nrow(ctAssoc1[[1]]$results) + nrow(ctAssoc2[[1]]$results)
    )
})
