test_that("magma_run works", {
  
    MAGMA.Celltyping::magma_run(cmd = "--version")
    testthat::expect_null(NULL)
})
