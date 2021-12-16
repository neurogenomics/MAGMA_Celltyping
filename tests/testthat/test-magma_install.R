test_that("magma_install/uninstall works", {
  
    #### Run first to ensure we're starting from scratch #### 
    uninstalled_prelim <- magma_uninstall()
    
    #### Install/uninstall: latest version ####
    ## Install
    installed <- magma_install(desired_version = "latest", 
                               upgrade = FALSE) 
    testthat::expect_true(is.character(installed))
    testthat::expect_length(installed, 1)
    testthat::expect_true(file.exists(installed))
    ## Uninstall
    uninstalled <- magma_uninstall()
    testthat::expect_true(is.character(uninstalled))
    testthat::expect_false(file.exists(uninstalled))
    
    uninstalled <- magma_uninstall()
    testthat::expect_length(uninstalled,0)
    
    #### Install/uninstall: multiple versions ####
    ## Install
    magma_install(desired_version = "latest") 
    magma_install(desired_version = "1.08b", 
                                    upgrade = TRUE) 
    ## Uninstall
    uninstalled <- magma_uninstall()
    testthat::expect_true(is.character(uninstalled))
    testthat::expect_length(uninstalled, 2)
    testthat::expect_false(all(file.exists(uninstalled)))
})
