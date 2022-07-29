test_that("import_magma_files works", {
  
    #### As directory ####
    magma_dirs <- MAGMA.Celltyping::import_magma_files(ids = c("ieu-a-298"), 
                                                       return_dir = TRUE)
    testthat::expect_true(dir.exists(magma_dirs))
    
    #### As file paths ####
    magma_files <- MAGMA.Celltyping::import_magma_files(ids = c("ieu-a-298"), 
                                                       return_dir = FALSE)
    testthat::expect_length(magma_files,2)
    testthat::expect_true(all(file.exists(magma_files)))
    
    #### As NESTED file paths ####
    magma_nfiles <- MAGMA.Celltyping::import_magma_files(ids = c("ieu-a-298"), 
                                                         nested = TRUE,
                                                         return_dir = FALSE)
    testthat::expect_true(methods::is(magma_nfiles,"list"))
    testthat::expect_length(unlist(magma_nfiles),2)
    testthat::expect_true(all(file.exists(unlist(magma_nfiles))))
    
    #### As tables ####
    magma_tables <- MAGMA.Celltyping::import_magma_files(ids = c("ieu-a-298"), 
                                                        return_tables = TRUE)
    testthat::expect_length(magma_tables[[1]],2) 
    testthat::expect_true(all(
        unlist(lapply(magma_tables[[1]],methods::is, "data.table"))
    ))
    
})
