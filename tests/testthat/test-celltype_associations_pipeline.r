test_that("celltype_associations_pipeline works", {
    
    ## celltype_associations_pipeline runs many of the main functions in 
    ## MAGMA.Celltyping, so this contributes a lot to test coverage
    
    #### Import precomputed MAGMA files ####
    ## MAGMA files were precomputed using the following steps:
    ## 1. MungeSumstats::import_sumstats()
    ## 2. MAGMA.Celltyping::map_snps_to_genes()
   
    
    if(!is_32bit()){
        run_tests <- function(res,
                              ctd,
                              ids){
            testthat::expect_length(unique(names(res)),length(ids))
            testthat::expect_true(startsWith(names(res)[1],ids[1])) 
            
            #### Iterate through results within each CTD level ####
            for(d in names(res)){
                message("\n",d)
                for(lvl in seq_len(length(ctd))){
                    message("Testing level: ",lvl)
                    #### Check that the magma_dir does exist ####
                    testthat::expect_true(file.exists(res[[d]]$magma_dir))
                    #### Check that all elements are present #### 
                    nms <- c("magma_dir","ctAssocsLinear","ctAssocsTop",
                             "ctAssocMerged","ctCondAssocs")
                    testthat::expect_true(
                        all(nms %in% names(res[[d]])))
                    #### Check that both elements are present: ctAssocsLinear####
                    lvl_res <- res[[d]]$ctAssocsLinear[[lvl]] 
                    testthat::expect_true(!is.null(lvl_res))
                    testthat::expect_true(
                        all(c("geneCovarFile","results") %in% names(lvl_res))
                    )
                    ##### Check result shave all the same celltype names ####
                    ## (after standardization)
                    fixed_celltypes <- EWCE::fix_celltype_names(
                        colnames(ctd[[lvl]]$mean_exp))
                    testthat::expect_true(
                        all(fixed_celltypes == lvl_res$results$Celltype))
                    #### Check results have all proper col names ####
                    cnames <- c("Celltype","OBS_GENES","BETA","BETA_STD",
                                "SE","P","level","Method",
                                "GCOV_FILE","CONTROL","CONTROL_label","log10p",
                                "genesOutCOND","EnrichmentMode")
                    testthat::expect_true(all(cnames %in% colnames(lvl_res$results)))
                }
            } 
        }
        #### Import data ####
        ids <- c("ieu-a-298")
        magma_dirs <- MAGMA.Celltyping::import_magma_files(ids = ids) 
        
        #### Using default ctd ####
        ctd <- ewceData::ctd()
        #### Run enrichment analyses ####
        res <- MAGMA.Celltyping::celltype_associations_pipeline(
            ctd = ctd, 
            ctd_name = "Zeisel2015", 
            run_linear = TRUE,
            run_top10 = TRUE,
            run_conditional = TRUE, 
            magma_dirs = magma_dirs,
            force_new = TRUE
        )
       run_tests(res = res,
                 ctd = ctd,
                 ids = ids)
       
       #### Using ctd_AIBS ####
       ctd <- get_ctd(ctd_name = "ctd_AIBS")
       #### Run enrichment analyses ####
       res <- MAGMA.Celltyping::celltype_associations_pipeline(
           ctd = ctd, 
           ctd_name = "AIBS",
           ctd_species = "human",
           run_linear = TRUE,
           run_top10 = TRUE,
           run_conditional = TRUE, 
           magma_dirs = magma_dirs,
           force_new = TRUE
       )
       run_tests(res = res,
                 ctd = ctd,
                 ids = ids)
       
       
       #### Using ctd_AIBS ####
       ctd <- get_ctd(ctd_name = "ctd_Aerts2018")
       #### Run enrichment analyses ####
       res <- MAGMA.Celltyping::celltype_associations_pipeline(
           ctd = ctd, 
           ctd_name = "Aerts2018",
           ctd_species = "human",
           run_linear = TRUE,
           run_top10 = TRUE,
           run_conditional = TRUE, 
           magma_dirs = magma_dirs,
           force_new = TRUE
       )
       run_tests(res = res,
                 ctd = ctd,
                 ids = ids)
    } else {
        testthat::expect_null(NULL)
    }
    
})
