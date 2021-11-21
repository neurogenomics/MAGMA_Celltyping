test_that("calculate_celltype_associations works", {
    #### import CTD ####
    ctd <- ewceData::ctd()
    # ctd <- MAGMA.Celltyping::get_ctd(ctd_name = "ctd_DRONC_mouse")
    
    ####  Method 1: starting from GWAS ####
    path_formatted <- MAGMA.Celltyping::get_example_gwas()
    ##### Map SNPs to genes ####
    genesOutPath <- MAGMA.Celltyping::map_snps_to_genes(
        path_formatted = path_formatted,
        genome_build = "GRCh37",
        N = 5000
    )
    #### Run pipeline ####
    ctAssocs <- MAGMA.Celltyping::calculate_celltype_associations(
        ctd = ctd,
        ctd_levels = 1,
        gwas_sumstats_path = path_formatted,
        ctd_species = "mouse"
    )
    
    
    #### Method 2: starting from MAGMA files ####
    #### Prepare data #### 
    magma_dir <- MAGMA.Celltyping::import_magma_files(
        ids = "ieu-a-298",
        return_dir = TRUE)
    #### Run pipeline ####
    ctAssocs <- MAGMA.Celltyping::calculate_celltype_associations(
        ctd = ctd,
        magma_dir = magma_dir,
        ctd_species = "mouse"
    )
})
