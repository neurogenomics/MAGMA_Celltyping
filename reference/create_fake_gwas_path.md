# Create a fake GWAS path when all you have are the MAGMA files

Necessary to trick
[calculate_celltype_associations](https://neurogenomics.github.io/MAGMA_Celltyping/reference/calculate_celltype_associations.md)
and
[calculate_conditional_celltype_associations](https://neurogenomics.github.io/MAGMA_Celltyping/reference/calculate_conditional_celltype_associations.md)
to take MAGMA files (".genes.raw" and ".genes.out") alone.

## Usage

``` r
create_fake_gwas_path(
  magma_dir,
  upstream_kb = 35,
  downstream_kb = 10,
  remove_pattern = "/MAGMA_Files/"
)
```

## Arguments

- upstream_kb:

  How many kb upstream of the gene should SNPs be included?

- downstream_kb:

  How many kb downstream of the gene should SNPs be included?
