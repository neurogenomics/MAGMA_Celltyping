# Map specificity to Entrez gene IDs

Convenience function used in
[create_gene_covar_file](https://neurogenomics.github.io/MAGMA_Celltyping/reference/create_gene_covar_file.md).

## Usage

``` r
map_specificity_to_entrez(
  ctd,
  annotLevel,
  ctd_species,
  use_matrix = "specificity_quantiles",
  return_ctd = FALSE,
  use_saved = TRUE,
  verbose = TRUE
)
```

## Arguments

- ctd:

  Cell type data structure containing `specificity_quantiles`.

- ctd_species:

  Species name relevant to the CellTypeDataset (`ctd`). See
  [list_species](https://rdrr.io/pkg/EWCE/man/list_species.html) for all
  available species. If `ctd_species=NULL` (default), the `ctd` species
  will automatically be inferred using
  [infer_species](https://rdrr.io/pkg/orthogene/man/infer_species.html).

- use_matrix:

  Which CTD matrix to use as input.

- return_ctd:

  Return the actual CellTypeDataset, rather than path to where it is
  saved.

- use_saved:

  Use a saved version of the gene mapping table.

- verbose:

  Print messages.

## Value

Matrix in which the first column is 'entrez' and then the specificity
decile for each cell type
