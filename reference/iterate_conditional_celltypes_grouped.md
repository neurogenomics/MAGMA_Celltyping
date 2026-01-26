# iterate_conditional_celltypes: grouped

Support function for
[calculate_conditional_celltype_associations](https://neurogenomics.github.io/MAGMA_Celltyping/reference/calculate_conditional_celltype_associations.md).

## Usage

``` r
iterate_conditional_celltypes_grouped(
  allRes,
  ctd,
  signifCells2,
  EnrichmentMode,
  annotLevel,
  controlledAnnotLevel,
  genesCovarFile,
  controlCovarFile,
  controlledCovarCols,
  magmaPaths,
  upstream_kb,
  downstream_kb,
  version,
  verbose
)
```

## Arguments

- allRes:

  Results from
  [`MAGMA.Celltyping::iterate_conditional_celltypes`](https://neurogenomics.github.io/MAGMA_Celltyping/reference/iterate_conditional_celltypes.md).

- ctd:

  Cell type data structure containing `specificity_quantiles`.

- EnrichmentMode:

  \[Optional\] Should either 'Linear' or 'Top 10%' mode be used for
  testing enrichment?

- controlledAnnotLevel:

  Which annotation level should be controlled for.

- upstream_kb:

  How many kb upstream of the gene should SNPs be included?

- downstream_kb:

  How many kb downstream of the gene should SNPs be included?

- version:

  MAGMA version to use.

- verbose:

  Print messages.
