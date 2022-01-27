test_that("magma_run works", {
  
    if(!is_32bit()){
        magma_x <- magma_install()
        magma_run(cmd = "--version") 
    } 
    testthat::expect_null(NULL)
})
