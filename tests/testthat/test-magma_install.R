test_that("magma_install works", {
  
    MAGMA.Celltyping::magma_install(desired_version = "latest", 
                                    upgrade = TRUE) 
    testthat::expect_null(NULL)
})
