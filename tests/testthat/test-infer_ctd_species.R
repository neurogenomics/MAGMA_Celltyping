test_that("infer_ctd_species works", {
    
    ctd <- ewceData::ctd()
    #### Inferred ####
    ctd_species <- MAGMA.Celltyping::infer_ctd_species(ctd = ctd)
    testthat::expect_equal(ctd_species,"mouse")
    
    #### User-supplied ####
    ctd_species <- MAGMA.Celltyping::infer_ctd_species(ctd = ctd, 
                                                       ctd_species = "dinosaur")
    testthat::expect_equal(ctd_species,"dinosaur") 
})
