test_that("Deprecation works", {
    
    expect_warn_error <- function(expr) {
        testthat::expect_warning(
            testthat::expect_error(
                force(expr)
            )
        )
    }
    
    if (!is_32bit()) {
        path <- MAGMA.Celltyping::get_example_gwas()
        first_line <- readLines(path)[1]
        
        expect_warn_error(
            MAGMA.Celltyping::get_genomebuild_for_sumstats(path = path)
        )
        expect_warn_error(
            MAGMA.Celltyping:::get_genomebuild_for_sumstats(path = path)
        )
        expect_warn_error(
            MAGMA.Celltyping::format_sumstats_for_magma(path = path)
        )
        expect_warn_error(
            MAGMA.Celltyping:::format_sumstats_for_magma_macOnly(path = path)
        )
        expect_warn_error(
            MAGMA.Celltyping::standardise.sumstats.column.headers(path = path)
        )
        expect_warn_error(
            MAGMA.Celltyping:::standardise.sumstats.column.headers.crossplatform(
                first_line = first_line
            )
        )
    } else {
        testthat::expect_null(NULL)
    }
})