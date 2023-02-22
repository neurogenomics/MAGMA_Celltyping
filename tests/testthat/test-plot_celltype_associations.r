test_that("plot_celltype_associations works", {
  
    res <- MAGMA.Celltyping::enrichment_results
    ctAssocs <- res$`finn-a-AD.tsv.gz.35UP.10DOWN`$ctAssocsLinear
    ctd <- ewceData::ctd()
    figs <- MAGMA.Celltyping::plot_celltype_associations(
        ctAssocs = ctAssocs,
        ctd = ctd)
    testthat::expect_length(figs,1)
    testthat::expect_true(methods::is(figs[[1]],"gg"))
})
