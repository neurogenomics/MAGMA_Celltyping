# Import preprocessed MAGMA files

Import preprocessed MAGMA SNPs-to-genes mapping files for many GWAS.
These files were generated using
[map_snps_to_genes](https://neurogenomics.github.io/MAGMA_Celltyping/reference/map_snps_to_genes.md)
and are publicly available via the [*MAGMA_Files_Public* GitHub
repository](https://github.com/neurogenomics/MAGMA_Files_Public).
Metadata for each dataset (including trait descriptions) can be found
here
[here](https://github.com/neurogenomics/MAGMA_Files_Public/blob/master/metadata.csv).

## Usage

``` r
import_magma_files(
  save_dir = tempdir(),
  ids = NULL,
  file_types = c(".genes.out", ".genes.raw"),
  overwrite = FALSE,
  return_dir = TRUE,
  return_tables = FALSE,
  nested = FALSE,
  nThread = 1,
  verbose = TRUE
)
```

## Source

[*MAGMA_Files_Public* GitHub
repository](https://github.com/neurogenomics/MAGMA_Files_Public)

## Arguments

- save_dir:

  Parent folder where you want to save the MAGMA files.

- ids:

  \[Optional\] [OpenGWAS](https://gwas.mrcieu.ac.uk/) dataset IDs. You
  see metadata for the GWAS
  [here](https://github.com/neurogenomics/MAGMA_Files_Public/blob/master/metadata.csv).

- file_types:

  MAGMA file types to import. Both ".genes.out" and ".genes.raw" are
  needed to run MAGMA.Celltyping cell-type enrichment functions

- overwrite:

  If the files have already been downloaded in the specified directory,
  these downloads will be skipped. Set `overwrite=TRUE` to force them to
  be re-downloaded (Default: `FALSE`).

- return_dir:

  Return a list of unique directory names instead of the full file paths
  (Default: `TRUE`).

- return_tables:

  Returns the files imported as tables (`TRUE`), instead of as paths
  (default: `FALSE`). Overrides `return_dir` argument.

- nested:

  Return files as a list nested by `id` (default: `FALSE`).

- nThread:

  Number of threads to parallelise downloads across.

- verbose:

  Print messages.

## Value

Named vector of paths to downloaded MAGMA files (or directories), or a
[data.table](https://rdrr.io/pkg/data.table/man/data.table.html).

## Examples

``` r
magma_dirs <- MAGMA.Celltyping::import_magma_files(ids = c("ieu-a-298"))
#> Using built-in example files: ieu-a-298.tsv.gz.35UP.10DOWN
#> Returning MAGMA directories.
```
