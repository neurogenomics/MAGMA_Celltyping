# Check input to MAGMA cell type analyses

This code was functonalised because it is called by both baseline and
conditional analysis.

## Usage

``` r
check_inputs_to_magma_celltype_analysis(
  ctd,
  gwas_sumstats_path,
  upstream_kb,
  downstream_kb
)
```

## Arguments

- ctd:

  Cell type data structure containing `specificity_quantiles`.

- gwas_sumstats_path:

  File path of the summary statistics file.

- upstream_kb:

  How many kb upstream of the gene should SNPs be included?

- downstream_kb:

  How many kb downstream of the gene should SNPs be included?

## Value

Nothing returned but sends error if there is a fault with the arguments.
