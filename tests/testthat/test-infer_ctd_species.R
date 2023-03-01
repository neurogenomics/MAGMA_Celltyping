test_that("infer_ctd_species works", {
    
    ctd <- ewceData::ctd()
    #### Inferred from genes ####
    ctd_species <- MAGMA.Celltyping::infer_ctd_species(ctd = ctd)
    testthat::expect_equal(ctd_species,"mouse")
    #### Inferred from metadata ####
    ctd[[1]]$species$output_species <- "mousey_mouse"
    ctd_species <- MAGMA.Celltyping::infer_ctd_species(ctd = ctd)
    testthat::expect_equal(ctd_species,"mousey_mouse")
    
    #### User-supplied ####
    ctd_species <- MAGMA.Celltyping::infer_ctd_species(ctd = ctd, 
                                                       ctd_species = "dinosaur")
    testthat::expect_equal(ctd_species,"dinosaur") 
})
