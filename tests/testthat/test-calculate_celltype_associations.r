test_that("calculate_celltype_associations works", {
    
        {  
            ctd <- ewceData::ctd()  
            magma_dir <- import_magma_files(ids = "ieu-a-298") 
        }
       run_tests <- function(ctAssocs, 
                             ctd){
           ## Test
           ctAssocs_names <- c("total_baseline_tests_performed",
                               "gwas_sumstats_path",
                               "analysis_name","upstream_kb","downstream_kb")
           testthat::expect_true(all(ctAssocs_names %in% names(ctAssocs)))
           ## Iterate through each CTD level 
           for(lvl in seq_len(length(ctd))){
               message("Testing level",lvl)
               celltypes <- EWCE::fix_celltype_names(
                   celltypes =  colnames(ctd[[lvl]]$specificity))
               testthat::expect_true(all(celltypes %in% 
                                             ctAssocs[[lvl]]$results$Celltype))
           } 
       }
    
       #### Linear mode #### 
       ctAssocs_linear <- calculate_celltype_associations(
           ctd = ctd, 
           magma_dir = magma_dir,
           EnrichmentMode = "Linear",
           ctd_species = "mouse", 
           force_new = TRUE
       )
       run_tests(ctAssocs = ctAssocs_linear,
                 ctd = ctd)
       
       #### Top 10% mode #### 
       ctAssocs_top10 <- calculate_celltype_associations(
           ctd = ctd, 
           magma_dir = magma_dir,
           EnrichmentMode = "Top 10%",
           ctd_species = "mouse",
           force_new = TRUE
       )
       run_tests(ctAssocs = ctAssocs_top10,
                 ctd = ctd)
       
       #### Linear: Conditional mode #### 
       magma_dir1 <- import_magma_files(ids = "Vuckovic2020.baso") 
       genesOutCOND <- import_magma_files(ids = "Vuckovic2020.baso_p", return_dir = FALSE) 
       ctAssocs_linear_cond <- calculate_celltype_associations(
           ctd = ctd, 
           magma_dir = magma_dir1,
           EnrichmentMode = "Linear",
           genesOutCOND = genesOutCOND[1],
           ctd_species = "mouse",
           force_new = TRUE
       ) 
       run_tests(ctAssocs = ctAssocs_linear_cond,
                 ctd = ctd)
       
       #### Top 10%: Conditional mode #### 
       ctAssocs_top10_cond <- calculate_celltype_associations(
           ctd = ctd, 
           magma_dir = magma_dir1,
           EnrichmentMode = "Top 10%",
           genesOutCOND = genesOutCOND[1],
           ctd_species = "mouse",
           force_new = TRUE
       ) 
       run_tests(ctAssocs = ctAssocs_top10_cond,
                 ctd = ctd)
})
