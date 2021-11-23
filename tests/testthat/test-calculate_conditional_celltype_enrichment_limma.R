test_that("calculate_conditional_celltype_enrichment_limma works", {
    
    ## Encompasses tests for:
    ## - calculate_conditional_celltype_enrichment_limma
    ## - adjust_zstat_in_genesOut
    ## - calculate_celltype_enrichment_limma
    ## ...since calculate_conditional_celltype_enrichment_limma
    ## uses the latter two.
    
    ctd <- ewceData::ctd()
    magma_cols <- c("entrez","GENE","CHR","START","STOP","NSNPS","NPARAM",
                    "N","ZSTAT","P","hgnc_symbol","Q","logNSNPS","logNPARAM",
                    "GENELEN","logGENELEN","ADJ_ZSTAT")
    
    #### MAGMA 1 #####
    genesOut1 <- MAGMA.Celltyping::import_magma_files(
        ids = c("ieu-a-298"),
        file_types = ".genes.out",
        return_dir = FALSE)
    magma1 <- MAGMA.Celltyping::adjust_zstat_in_genesOut(
        ctd = ctd,
        magma_GenesOut_file = genesOut1)
    testthat::expect_true(file.exists(genesOut1))
    testthat::expect_true(all(magma_cols %in% colnames(magma1)))

    #### MAGMA 2 #####
    genesOut2 <- MAGMA.Celltyping::import_magma_files(
        ids = c("ukb-b-6548"),
        file_types = ".genes.out",
        return_dir = FALSE)
    magma2 <- MAGMA.Celltyping::adjust_zstat_in_genesOut(
        ctd = ctd,
        magma_GenesOut_file = genesOut2)
    testthat::expect_true(file.exists(genesOut2))
    testthat::expect_true(all(magma_cols %in% colnames(magma2)))

    #### Run conditional enrichment ####
    annotLevel <- 1
    cond_res <- MAGMA.Celltyping::calculate_conditional_celltype_enrichment_limma(
        magma1 = magma1,
        magma2 = magma2,
        annotLevel = annotLevel,
        ctd = ctd)
    
    testthat::expect_true(
        all(c("Celltype","ps","coef","p1_baseline","p2_baseline","qs") %in% 
            colnames(cond_res)))
    celltypes <- EWCE::fix_celltype_names(
        celltypes = colnames(ctd[[annotLevel]]$specificity))
    testthat::expect_equal(sort(cond_res$Celltype), sort(celltypes))
})
