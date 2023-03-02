test_that("check_n works", {
  
    # Tutorial on testing interactive functions here:
    # https://debruine.github.io/post/interactive-test/
    
    wd <- getwd()
    path_formatted <- MAGMA.Celltyping::get_example_gwas(
        trait = "educational_attainment",
        storage_dir = wd)
    
    run_input <- function(lines){ 
        # set up interactive answers
        f <- file()
        ans <- paste(lines, collapse = "\n")
        write(ans, f)
        options("ask_opts.con" = f) # set connection option 
        
        output_prompts <- testthat::capture_output_lines({
            check_n(path_formatted = path_formatted, N = NULL)
            # ask_integer(prompt = "Enter here")
        }) 
        close(f) # close the file 
        options("ask_opts.con" = stdin()) # reset connection option
        return(output_prompts)
    }
    prompt <- paste("There is no N column within the sumstats file.",
                    "What is the N value for this GWAS? [enter integer]")
    invalid <- "\U000274C Must provide an integer"
    valid <- "\U0002705 Valid integer detected."
    error <-  "Error in check_n(path_formatted = path_formatted, N = NULL): Value of N provided is less than 1,000. This seems unlikely.\n"
    
    out <- run_input(lines = list(NULL, "typo",1000))
    testthat::expect_equal(out,
                           c(prompt,invalid,
                             prompt,invalid, 
                             prompt,valid))
    out <- run_input(lines = list("1000"))
    testthat::expect_equal(out,
                           c(prompt,valid)) 
    out2 <- run_input(lines = list(10000))
    testthat::expect_equal(out2,
                           c(prompt,valid))
    error_out <- testthat::capture_error(
        run_input(lines = list(100))
    )
    testthat::expect_equal(as.character(error_out),
                           error)
})
