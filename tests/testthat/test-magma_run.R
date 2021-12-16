test_that("magma_run works", {
  
    if(!is_32bit()){
        MAGMA.Celltyping::magma_install()
        MAGMA.Celltyping::magma_run(cmd = "--version") 
    } 
    testthat::expect_null(NULL)
})
