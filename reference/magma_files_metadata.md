# MAGMA_Files_Public: metadata

Metadata for all pre-computed metadata files stored in the
[MAGMA_Files_Public](https://github.com/neurogenomics/MAGMA_Files_Public)
GitHub repository.

## Usage

``` r
data("magma_files_metadata")
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 54
rows and 29 columns.

## Source

` #### Check what files are available #### magma_files_metadata <- data.table::fread( paste("https://github.com/neurogenomics/MAGMA_Files_Public", "raw/master/metadata.csv", sep="/") ) usethis::use_data(magma_files_metadata, overwrite = TRUE) `
