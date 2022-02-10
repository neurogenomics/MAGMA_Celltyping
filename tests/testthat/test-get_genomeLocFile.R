test_that("get_genomeLocFile works", {
    
    if(!is_32bit()){
        refs <- c("GRCH36","GRCH37","GRCH38")
        for(x in refs){
            message(x)
            tmp <- MAGMA.Celltyping:::get_genomeLocFile(build = x)
            d <- data.table::fread(tmp)
            testthat::expect_gte(nrow(d),18000)
            testthat::expect_equal(ncol(d),6)
        }
        testthat::expect_error(
            MAGMA.Celltyping:::get_genomeLocFile(build = "typo")
            )
    } else {
        testthat::expect_null(NULL)
    } 
})
