test_that("adjust_zstat_in_genesOut works", {
    
    myGenesOut <- MAGMA.Celltyping::import_magma_files(
        ids = c("ieu-a-298"),
        file_types = ".genes.out", 
        return_dir = FALSE)[1]
    ctd <- ewceData::ctd()

    magmaGenesOut <- MAGMA.Celltyping::adjust_zstat_in_genesOut(
        ctd = ctd,
        magma_GenesOut_file = myGenesOut,
        ctd_species = "mouse"
    )
    genesOut_dt <- data.table::fread(myGenesOut)
    
    testthat::expect_gte(nrow(genesOut_dt), 1300)
    testthat::expect_equal(ncol(genesOut_dt), 9)
    testthat::expect_gte(nrow(magmaGenesOut), 900)
    testthat::expect_equal(ncol(magmaGenesOut), 17)
    res_cols <- c("entrez","GENE","CHR","START","STOP","NSNPS",   
      "NPARAM","N","ZSTAT","P","hgnc_symbol","Q",
      "logNSNPS","logNPARAM","GENELEN","logGENELEN","ADJ_ZSTAT")
    testthat::expect_true(all(res_cols %in% colnames(magmaGenesOut)))
})
