test_that("adjust_zstat_in_genesOut works", {
 
    myGenesOut <- MAGMA.Celltyping::import_magma_files(
        ids = c("ieu-a-298"),
        file_types = ".genes.out",
        return_dir = FALSE)
    ctd <- ewceData::ctd()
    cols <- c("Q","logNSNPS","logNPARAM", "GENELEN","logGENELEN","ADJ_ZSTAT")

    #### With CTD #####
    magmaGenesOut <- MAGMA.Celltyping::adjust_zstat_in_genesOut( 
        magma_GenesOut_file = myGenesOut,
        ctd = ctd,
        ctd_species = "mouse"
    )
    testthat::expect_true(methods::is(magmaGenesOut,"data.table"))
    testthat::expect_true(all(cols %in% names(magmaGenesOut)))
    testthat::expect_true(nrow(magmaGenesOut)==965)
    
    #### Without CTD #####
    magmaGenesOut2 <- MAGMA.Celltyping::adjust_zstat_in_genesOut( 
        magma_GenesOut_file = myGenesOut
    )
    testthat::expect_true(methods::is(magmaGenesOut2,"data.table"))
    testthat::expect_true(all(cols %in% names(magmaGenesOut2)))
    testthat::expect_true(nrow(magmaGenesOut2)==1336)
})
