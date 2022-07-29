test_that("infer_geneset_species works", {
  
    geneset <- MAGMA.Celltyping::rbfox_binding
    #### Inferred ####
    geneset_species <- infer_geneset_species(geneset = geneset)
    testthat::expect_equal(geneset_species,"mouse")
    
    #### User-supplied ####
    geneset_species <- infer_geneset_species(geneset = geneset,
                                             geneset_species = "dinosaur")
    testthat::expect_equal(geneset_species,"dinosaur")
})
