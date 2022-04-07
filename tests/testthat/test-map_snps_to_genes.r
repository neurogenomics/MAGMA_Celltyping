test_that("map_snps_to_genes works", {
    
    if(!is_32bit()){
        t1 <- Sys.time()
        #### Gather data ####
        ctd <- ewceData::ctd()
        #### Run SNP-to-gene mapping ####
        ## Use a very small example GWAS file
        path_formatted <- MAGMA.Celltyping::get_example_gwas(
            trait = "educational_attainment")
        ## Map
        genesOutPath <- MAGMA.Celltyping::map_snps_to_genes(
            path_formatted = path_formatted,
            genome_build = "hg19",
            N = 5000)
        ## Test
        genesOut_cols <- c("GENE","CHR","START","STOP","NSNPS",
                           "NPARAM","N","ZSTAT","P")
        testthat::expect_true(file.exists(genesOutPath))
        testthat::expect_true(file.exists(gsub(".out",".raw",genesOutPath)))
        genesOut_dt <- data.table::fread(MAGMA.Celltyping:::fix_path(genesOutPath))
        testthat::expect_true(all(genesOut_cols %in% colnames(genesOut_dt)))
        testthat::expect_true(nrow(genesOut_dt)>70)

        #### Run enrichment pipeline ####
        ## Run
        ctd_levels = 1
        ctAssocs <- MAGMA.Celltyping::calculate_celltype_associations(
            ctd = ctd,
            ctd_levels = ctd_levels,
            gwas_sumstats_path = path_formatted,
            ctd_species = "mouse"
        )
        ## Test
        ctAssocs_names <- c("total_baseline_tests_performed","gwas_sumstats_path",
                            "analysis_name","upstream_kb","downstream_kb")
        testthat::expect_true(all(ctAssocs_names %in% names(ctAssocs)))
        celltypes <- EWCE::fix_celltype_names(
            celltypes =  colnames(ctd[[1]]$specificity))
        testthat::expect_true(all(celltypes %in% ctAssocs[[1]]$results$Celltype))
        
        t2 <- Sys.time()
        print(t2-t1)
    } else {testthat::expect_null(NULL)}
})
