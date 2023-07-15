test_that("map_snps_to_genes works", {
    
    if(!is_32bit() & (.Platform$OS.type != "windows")){
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
        genesOut_dt <- data.table::fread(file = genesOutPath)
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
         
        #### Bi-allelic vs. Multi-allelic GWAS sumstats ####
        ### Set dbSNP version
        # dbSNP <- 144 # 155
        # ## Munge
        # path_formatted1 <- MungeSumstats::import_sumstats(
        #   ids = "ieu-a-8",
        #   ref_genome = "GRCh37",
        #   dbSNP = dbSNP,
        #   save_dir = "~/Downloads/biallelic",
        #   bi_allelic_filter = TRUE)
        # ## Map
        # genesOutPath1 <- MAGMA.Celltyping::map_snps_to_genes(
        #   path_formatted = path_formatted1[[1]],
        #   # force_new = TRUE,
        #   duplicate = "first",
        #   synonym_dup = "skip-dup",
        #   genome_build = "hg19") 
        # ## With multi-allelic SNPs
        # ## Munge
        # path_formatted2 <- MungeSumstats::import_sumstats(
        #   ids = "ieu-a-8",
        #   ref_genome = "GRCh37",
        #   dbSNP = dbSNP,
        #   save_dir = "~/Downloads/mutliallelic",
        #   allele_flip_frq = FALSE,
        #   bi_allelic_filter = FALSE)
        # ## Map
        # genesOutPath2 <- MAGMA.Celltyping::map_snps_to_genes(
        #   path_formatted = path_formatted2[[1]],
        #   # force_new = TRUE,
        #   duplicate = "first",
        #   synonym_dup = "skip-dup",
        #   genome_build = "hg19") 
        # ## Compare GWAS files 
        # dt1 <- data.table::fread( path_formatted1[[1]])
        # dt2 <- data.table::fread( path_formatted2[[1]])
        # testthat::expect_lt(nrow(dt1), nrow(dt2))
        # ## Check for duplicated RSIDS
        # testthat::expect_equal(sum(duplicated(dt1$SNP)),0)
        # testthat::expect_equal(sum(duplicated(dt2$SNP)),0)
        # ## Check number of SNPs from 1KG ref in each GWAS file
        # f <- list.files(
        #   path = "/Users/schilder/Library/Caches/org.R-project.R/R/MAGMA.Celltyping/g1000_eur/",
        #   pattern = "bed$|bim$|fam$",
        #   full.names = TRUE)
        # bed <- plinkr::read_plink_bed_file_from_files(
        #   bed_filename = f[1],
        #   bim_filename = f[2], 
        #   fam_filename = f[3])
        # f_syn <- list.files(
        #   path = "/Users/schilder/Library/Caches/org.R-project.R/R/MAGMA.Celltyping/g1000_eur/",
        #   pattern = "synonyms$", 
        #   full.names = TRUE)
        # syn <- readLines(f_syn)[-c(1,2)]
        # snps_syn <- paste0("rs",unlist(strsplit(syn," "))) 
        # snps <- unique(c(rownames(bed),snps_syn))
        # snps1 <- intersect(snps,dt1$SNP)
        # snps2 <- intersect(snps,dt2$SNP)
        # testthat::expect_false(length(snps1)==length(snps2))
        # length(snps2)- length(snps1)
        # ## Compare MAGMA files 
        # g1 <- MAGMA.Celltyping:::read_magma_genes_out(genesOutPath1)
        # g2 <- MAGMA.Celltyping:::read_magma_genes_out(genesOutPath2)
        # all.equal(g1,g2) 
        
    } else {testthat::expect_null(NULL)}
})
