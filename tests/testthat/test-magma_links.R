test_that("magma_links works", {
    
    if(!is_32bit()){
        to_numeric <- function(x){
            as.numeric(
                gsub(
                    paste( c(letters,".zip"), collapse = "|" ),"",
                    stringr::str_split(basename(x),"_")[[1]][2]
                    )
                )
        }
        
        #### Mac ####
        magma_mac <- MAGMA.Celltyping:::magma_links(latest_only = TRUE,
                               os = "Mac",
                               verbose = FALSE)
        testthat::expect_gte(to_numeric(magma_mac), 1.1) 
        
        #### Windows ####
        magma_win <- MAGMA.Celltyping:::magma_links(latest_only = TRUE,
                                                    os = "Windows")
        testthat::expect_gte(to_numeric(magma_win), 1.1) 
        
        #### Linux ####
        magma_linux <- MAGMA.Celltyping:::magma_links(latest_only = TRUE,
                                                      os = "Linux")
        testthat::expect_gte(to_numeric(magma_linux), 1.1) 
          
        #### All ####
        meta <- MAGMA.Celltyping:::magma_links(latest_only = FALSE,
                                               os = "Mac",
                                               return_table = TRUE)
        testthat::expect_gte(nrow(meta),19)
    } else {
        testthat::expect_null(NULL)
    } 
})
