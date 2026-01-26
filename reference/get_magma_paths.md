# Get MAGMA Paths

Gets paths to folders based on location of the GWAS summary statistics
file. This avoids the GWAS sumstats folder becoming a mess.

## Usage

``` r
get_magma_paths(
  gwas_sumstats_path = NA,
  upstream_kb = NA,
  downstream_kb = NA,
  output_path = NA
)
```

## Source

` gwas_sumstats_path <- MAGMA.Celltyping::get_example_gwas( trait = "educational_attainment") magmaPaths <- MAGMA.Celltyping:::get_magma_paths( gwas_sumstats_path = gwas_sumstats_path, upstream_kb = 35, downstream_kb = 10) `

## Arguments

- gwas_sumstats_path:

  Path to the GWAS summary statistics file.

- upstream_kb:

  How many kb upstream of the gene should SNPs be included?

- downstream_kb:

  How many kb downstream of the gene should SNPs be included?

- output_path:

  If being called from
  [magma_tileplot](https://neurogenomics.github.io/MAGMA_Celltyping/reference/magma_tileplot.md)
  then `gwas_sumstats_path` cannot be specified, so instead specificity
  the base folder where MAGMA_Figures exists.

## Value

File path for where MAGMA files should be created
