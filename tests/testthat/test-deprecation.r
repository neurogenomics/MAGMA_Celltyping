test_that("Deprecation works", {
    
    ## Ensure deprecated functions return stop messages.
    if(!is_32bit()){
        path <- MAGMA.Celltyping::get_example_gwas()
        first_line <- readLines(path)[1]
        testthat::expect_error(MAGMA.Celltyping::get_genomebuild_for_sumstats(path=path))
        testthat::expect_error(MAGMA.Celltyping:::get_genomebuild_for_sumstats(path = path))
        testthat::expect_error(MAGMA.Celltyping::format_sumstats_for_magma(path = path))
        testthat::expect_error(MAGMA.Celltyping::format.sumstats.for.magma(path = path))
        testthat::expect_error(MAGMA.Celltyping:::format_sumstats_for_magma_macOnly(path = path))
        testthat::expect_error(MAGMA.Celltyping::standardise.sumstats.column.headers(path = path))
        testthat::expect_error(MAGMA.Celltyping:::standardise.sumstats.column.headers.crossplatform(first_line = first_line))
    } else {
        testthat::expect_null(NULL)
    } 
})
