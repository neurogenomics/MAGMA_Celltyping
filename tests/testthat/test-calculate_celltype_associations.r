test_that("calculate_celltype_associations works", {
    
    {
        start <- Sys.time()
        #### import CTD ####
        ctd <- ewceData::ctd()
        # ctd <- MAGMA.Celltyping::get_ctd(ctd_name = "ctd_DRONC_mouse")
        
        #### Starting from MAGMA files ####
        #### Prepare data #### 
        magma_dir <- MAGMA.Celltyping::import_magma_files(
            ids = "ieu-a-298")
        #### Run pipeline ####
        ctAssocs <- MAGMA.Celltyping::calculate_celltype_associations(
            ctd = ctd, 
            magma_dir = magma_dir,
            ctd_species = "mouse"
        )
        print(Sys.time() - start)
    }
    ## Test
    ctAssocs_names <- c("total_baseline_tests_performed","gwas_sumstats_path",
                        "analysis_name","upstream_kb","downstream_kb")
    testthat::expect_true(all(ctAssocs_names %in% names(ctAssocs)))
    ## Iterate through each CTD level 
    for(lvl in seq_len(length(ctd))){
        message("Testing level",lvl)
        celltypes <- EWCE::fix_celltype_names(
            celltypes =  colnames(ctd[[lvl]]$specificity))
        testthat::expect_true(all(celltypes %in% ctAssocs[[lvl]]$results$Celltype))
    }
})
