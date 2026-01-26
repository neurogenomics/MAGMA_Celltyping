# Create gene covariance file: by mode

Create gene covariance file for a given `EnrichmentMode`.

## Usage

``` r
create_gene_covar_file_mode(
  EnrichmentMode,
  magmaPaths,
  ctd,
  annotLevel,
  ctd_species
)
```

## Arguments

- EnrichmentMode:

  \[Optional\] Should either 'Linear' or 'Top 10%' mode be used for
  testing enrichment?

- magmaPaths:

  Output of `get_magma_paths`.

- ctd:

  Cell type data structure containing `specificity_quantiles`.

- ctd_species:

  Species name relevant to the CellTypeDataset (`ctd`). See
  [list_species](https://rdrr.io/pkg/EWCE/man/list_species.html) for all
  available species. If `ctd_species=NULL` (default), the `ctd` species
  will automatically be inferred using
  [infer_species](https://rdrr.io/pkg/orthogene/man/infer_species.html).
