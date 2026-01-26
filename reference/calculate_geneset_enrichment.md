# Use MAGMA to test enrichment in a geneset

Assumes that you have already run
[map_snps_to_genes](https://neurogenomics.github.io/MAGMA_Celltyping/reference/map_snps_to_genes.md).

## Usage

``` r
calculate_geneset_enrichment(
  geneset,
  gwas_sumstats_path = NULL,
  magma_dir = NULL,
  analysis_name,
  upstream_kb = 35,
  downstream_kb = 10,
  geneset_species = "mouse",
  version = NULL,
  verbose = TRUE
)
```

## Arguments

- geneset:

  Genes which are to be tested. Can be gene symbols (or other gene IDs)
  from any species listed in
  [list_species](https://rdrr.io/pkg/EWCE/man/list_species.html).

- gwas_sumstats_path:

  File path of the summary statistics file.

- magma_dir:

  Path to folder containing the pre-computed MAGMA GWAS files
  (*.gsa.raw*and *.gsa.out*).

- analysis_name:

  Used in file names which area created.

- upstream_kb:

  How many kb upstream of the gene should SNPs be included?

- downstream_kb:

  How many kb downstream of the gene should SNPs be included?

- geneset_species:

  Species that the `geneset` came from. Can be any species listed in
  [list_species](https://rdrr.io/pkg/EWCE/man/list_species.html). If
  `geneset_species!="human"`, the `geneset` will be converted to 1:1
  human orthologs using
  [convert_orthologs](https://rdrr.io/pkg/orthogene/man/convert_orthologs.html).

- version:

  MAGMA version to use.

- verbose:

  Print messages.

## Value

Filepath for the genes.out file.
