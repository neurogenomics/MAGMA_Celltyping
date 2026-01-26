# Import MAGMA files metadata

Import metadata from the SNP-to-gene mapping files available in
[MAMGA_Files_Public](https://github.com/neurogenomics/MAGMA_Files_Public).

## Usage

``` r
import_magma_files_metadata(file_types, use_local = TRUE, verbose = TRUE)
```

## Arguments

- file_types:

  MAGMA file types to import. Both ".genes.out" and ".genes.raw" are
  needed to run MAGMA.Celltyping cell-type enrichment functions

- use_local:

  Whether to use a local copy of the metadata.

  - `TRUE`Use the metadata included with MAGMA.Celltyping (less
    frequently updated).

  - `FALSE`Import metadata from the GitHub repository (more frequently
    updated).

- verbose:

  Print messages.
