# Example celltype enrichment results

Example results from
[celltype_associations_pipeline](https://neurogenomics.github.io/MAGMA_Celltyping/reference/celltype_associations_pipeline.md).

## Usage

``` r
data("enrichment_results")
```

## Format

An object of class `list` of length 1.

## Source

` #### Import data #### magma_dirs <- MAGMA.Celltyping::import_magma_files(ids = "ieu-a-298") ctd <- ewceData::ctd() enrichment_results <- MAGMA.Celltyping::celltype_associations_pipeline( magma_dirs = magma_dirs, ctd = ctd, ctd_species = "mouse", ctd_name = "Zeisel2015", run_linear = TRUE, run_top10 = TRUE, run_conditional = TRUE, force_new = TRUE) usethis::use_data(enrichment_results, overwrite = TRUE) `
