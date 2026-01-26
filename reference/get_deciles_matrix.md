# Get deciles matrix

Extract the matrix of specificity deciles from a CellTypeDataset object.
Support function for
[create_top10percent_genesets_file](https://neurogenomics.github.io/MAGMA_Celltyping/reference/create_top10percent_genesets_file.md).

## Usage

``` r
get_deciles_matrix(ctd, annotLevel, ctd_species)
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
