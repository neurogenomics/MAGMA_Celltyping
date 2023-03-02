test_that("load_magma_results_file works", {
  
    path <- system.file(
        "extdata","ieu-a-298.tsv.gz.35UP.10DOWN.level1.MainRun.gsa.out",
        package = "MAGMA.Celltyping")
    ctd <- ewceData::ctd()
    annotLevel <- 1
    EnrichmentMode <- "Linear"
    res <- load_magma_results_file(path = path,
                                   ctd = ctd,
                                   annotLevel = annotLevel,
                                   EnrichmentMode = EnrichmentMode)
    ctd_celltypes <- EWCE::fix_celltype_names(colnames(ctd[[annotLevel]]$mean_exp))
    testthat::expect_equal(nrow(res), length(ctd_celltypes))
    testthat::expect_true(all(ctd_celltypes %in% res$Celltype))
})
