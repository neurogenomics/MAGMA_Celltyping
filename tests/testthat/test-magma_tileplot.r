test_that("magma_tileplot works", {
  
    res <- MAGMA.Celltyping::enrichment_results[[1]]
    results <- res$ctAssocMerged$level1$results
    ctd <- ewceData::ctd()
    #### Bound together ####
    tile_res <- magma_tileplot(ctd=ctd, 
                               results=results)
    testthat::expect_true(methods::is(tile_res,"patchwork"))
    
    #### Separated ####
    tile_res2 <- magma_tileplot(ctd=ctd,
                                results=results, 
                                bind_plots = FALSE)
    testthat::expect_true(methods::is(tile_res2$heatmap,"gg"))
    testthat::expect_true(methods::is(tile_res2$dendro,"gg"))
})
