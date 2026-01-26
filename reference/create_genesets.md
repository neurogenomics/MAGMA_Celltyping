# Create gene sets

Support function for
[get_driver_genes](https://neurogenomics.github.io/MAGMA_Celltyping/reference/get_driver_genes.md).

## Usage

``` r
create_genesets(
  res_input,
  n_genes = 100,
  spec_deciles = NULL,
  gene_col = "hgnc_symbol",
  verbose = TRUE
)
```

## Arguments

- res_input:

  Enrichment results output from
  [calculate_celltype_enrichment_limma](https://neurogenomics.github.io/MAGMA_Celltyping/reference/calculate_celltype_enrichment_limma.md).

- n_genes:

  Max number of genes to return.

- spec_deciles:

  \[Optional\] Which specificity deciles to use from the
  CellTypeDataset.

- gene_col:

  Column containing gene symbols.

- verbose:

  Print messages.
