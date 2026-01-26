# Calculate celltype associations using MAGMA

Assumes that you have already run
[map_snps_to_genes](https://neurogenomics.github.io/MAGMA_Celltyping/reference/map_snps_to_genes.md).

## Usage

``` r
calculate_celltype_associations(
  ctd,
  ctd_levels = seq_len(length(ctd)),
  ctd_species = infer_ctd_species(ctd),
  gwas_sumstats_path = NULL,
  magma_dir = NULL,
  analysis_name = "MainRun",
  prepare_ctd = TRUE,
  upstream_kb = 35,
  downstream_kb = 10,
  genesOutCOND = NA,
  EnrichmentMode = "Linear",
  force_new = FALSE,
  version = NULL,
  verbose = TRUE
)
```

## Arguments

- ctd:

  Cell type data structure containing `specificity_quantiles`.

- ctd_levels:

  Which levels of `ctd` to iterate the enrichment analysis over.

- ctd_species:

  Species name relevant to the CellTypeDataset (`ctd`). See
  [list_species](https://rdrr.io/pkg/EWCE/man/list_species.html) for all
  available species. If `ctd_species=NULL` (default), the `ctd` species
  will automatically be inferred using
  [infer_species](https://rdrr.io/pkg/orthogene/man/infer_species.html).

- gwas_sumstats_path:

  File path of the summary statistics file.

- magma_dir:

  Path to folder containing the pre-computed MAGMA GWAS files
  (*.gsa.raw*and *.gsa.out*).

- analysis_name:

  Used in file names which area created.

- prepare_ctd:

  Whether to run
  [prepare_quantile_groups](https://neurogenomics.github.io/MAGMA_Celltyping/reference/prepare_quantile_groups.md)
  on the `ctd` first.

- upstream_kb:

  How many kb upstream of the gene should SNPs be included?

- downstream_kb:

  How many kb downstream of the gene should SNPs be included?

- genesOutCOND:

  \[Optional\] Path to a genes.out file to condition on. Used if you
  want to condition on a different GWAS.

- EnrichmentMode:

  \[Optional\] Should either 'Linear' or 'Top 10%' mode be used for
  testing enrichment?

- force_new:

  \[Optional\] Force new MAGMA analyses even if the pre-existing results
  files are detected.

- version:

  MAGMA version to use.

- verbose:

  Print messages.

## Value

File path for the genes.out file

## Examples

``` r
#### Prepare cell-type data ####
ctd <- ewceData::ctd()
#> see ?ewceData and browseVignettes('ewceData') for documentation
#> loading from cache

#### Prepare GWAS MAGMA data ####
magma_dir <- MAGMA.Celltyping::import_magma_files(ids = "ieu-a-298")
#> Using built-in example files: ieu-a-298.tsv.gz.35UP.10DOWN
#> Returning MAGMA directories.
    
#### Run pipeline ####
ctAssocs <- calculate_celltype_associations(
    ctd = ctd,
    ctd_levels = 1,
    magma_dir = magma_dir,
    ctd_species = "mouse")
#> Installed MAGMA version: v1.10
#> Skipping MAGMA installation.
#> The desired_version of MAGMA is currently installed: v1.10
#> Using: magma_v1.10
#> Standardising CellTypeDataset
#> Found 5 matrix types across 2 CTD levels.
#> Processing level: 1
#> Processing level: 2
#> Converting to sparse matrix.
#> Converting to sparse matrix.
#> Running MAGMA: Linear mode
#> Mapping gene symbols in specificity_quantiles matrix to entrez IDs.
#> Reading enrichment results file into R.
```
