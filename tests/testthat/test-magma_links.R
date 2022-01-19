test_that("magma_links works", {
    
    if(!is_32bit()){
        #### Mac ####
        magma_mac <- MAGMA.Celltyping:::magma_links(latest_only = TRUE,
                               os = "Mac",
                               verbose = FALSE)
        testthat::expect_equal(names(magma_mac),"magma_v1.09b_mac") 
        
        #### Windows ####
        magma_win <- MAGMA.Celltyping:::magma_links(latest_only = TRUE,
                                                    os = "Windows")
        testthat::expect_equal(names(magma_win),"magma_v1.09b_win") 
        
        #### Linux ####
        magma_linux <- MAGMA.Celltyping:::magma_links(latest_only = TRUE,
                                                      os = "Linux")
        testthat::expect_equal(names(magma_linux),"magma_v1.09b") 
        
        #### All ####
        meta <- MAGMA.Celltyping:::magma_links(latest_only = FALSE,
                                               os = "Mac",
                                               return_table = TRUE)
        testthat::expect_gte(nrow(meta),19)
    } else {
        testthat::expect_null(NULL)
    } 
})
