test_that("create_top10percent_genesets_file works", {
    
    ctd <- ewceData::ctd()
    annotLevel <- 1
    #### Prepare GWAS MAGMA data ####
    magma_dir <- MAGMA.Celltyping::import_magma_files(ids = "ieu-a-298",
                                                      file_types = ".genes.out",
                                                      return_dir = FALSE)

    ctd <- MAGMA.Celltyping::prepare_quantile_groups(ctd = ctd)
    geneSetsFilePath <- MAGMA.Celltyping::create_top10percent_genesets_file(
        genesOutFile = myGenesOut,
        ctd = ctd,
        annotLevel = annotLevel,
        ## Mapped to human orths by prepare_quantile_groups
        ctd_species = "human"
    )
    geneSets <- readLines(geneSetsFilePath)
    #### One line per cell-type ####
    testthat::expect_equal(
        length(geneSets),
        ncol(ctd[[annotLevel]]$mean_exp)
    )
    #### That cell-type names are recoverable #### 
    celltypes <- unlist(lapply(geneSets, function(x) {
        strsplit(x, " ")[[1]][1]
    }))
    testthat::expect_true(
        all(celltypes %in% colnames(ctd[[annotLevel]]$mean_exp))
    ) 
})
