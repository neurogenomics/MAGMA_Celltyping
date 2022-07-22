test_that("get_ctd works", {
  
    #### One CTD ####
    ctd <- MAGMA.Celltyping::get_ctd(ctd_name = "ctd_AIBS")
    testthat::expect_true(EWCE:::is_celltypedataset(ctd))
    #### Multiple CTD ####
    ctd_name <-c("ctd_AIBS","ctd_allKI")
    ctd_list <- MAGMA.Celltyping::get_ctd(ctd_name = ctd_name)
    testthat::expect_true(all(
        unlist(lapply(ctd_list,EWCE:::is_celltypedataset)))
    ) 
    #### Incorrect CTD ####
    testthat::expect_error(
        MAGMA.Celltyping::get_ctd(ctd_name = "typooo")
    )
})
