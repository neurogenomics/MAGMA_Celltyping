test_that("plot_celltype_associations works", {
  
    res <- MAGMA.Celltyping::enrichment_results
    ctAssocs <- res[[1]]$ctAssocsLinear
    ctd <- ewceData::ctd()
    figs <- MAGMA.Celltyping::plot_celltype_associations(
        ctAssocs = ctAssocs,
        ctd = ctd)
    testthat::expect_length(figs,2)
    testthat::expect_true(methods::is(figs[[1]],"gg"))
    
    #### Bind together as one plot ####
    figs2 <- MAGMA.Celltyping::plot_celltype_associations(
        ctAssocs = ctAssocs,
        ctd = ctd,
        bind_plots = TRUE, 
        show_plot = FALSE)
    testthat::expect_true(methods::is(figs2,"gg"))
    
    
    
    figs3 <- MAGMA.Celltyping::plot_celltype_associations(
        ctAssocs = ctAssocs,
        ctd = ctd,
        plotDendro = FALSE,
        bind_plots = TRUE, 
        show_plot = FALSE)
    testthat::expect_true(methods::is(figs3,"gg"))
})
