skip_if_offline()
test_that("get_example_gwas non-munge/raw works", {
    gwas <- get_example_gwas(munged=FALSE,
                             trait="fluid_intelligence",
                             verbose=FALSE)
    
    # Ensure file exists
    expect_true(file.exists(gwas))
    
    # Inspect output
    gwas_df <- utils::read.csv(gwas, sep="\t", nrows=20)
    
    bases = c("A", "T", "C", "G")
    expect_contains(unique(gwas_df$minor_allele), bases)
})
