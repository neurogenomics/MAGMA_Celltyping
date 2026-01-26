# Create gene covariance file

The gene covariannce (covar) file is the input to MAGMA for the celltype
association analysis. This code was functonalised because it is called
by both baseline and conditional analysis.

## Usage

``` r
create_gene_covar_file(
  genesOutFile,
  ctd,
  annotLevel,
  ctd_species,
  genesOutCOND = NA,
  verbose = TRUE
)
```

## Source

` #### Example usage #### ctd <- ewceData::ctd() genesOutFile <- MAGMA.Celltyping::import_magma_files( ids = "ieu-a-298", file_types = ".genes.out$", return_dir = FALSE)genesCovarFilePath <- MAGMA.Celltyping:::create_gene_covar_file( genesOutFile = genesOutFile, ctd = ctd, annotLevel = 1, ctd_species = "mouse") `

## Arguments

- genesOutFile:

  The output of the second call to MAGMA (performed in the
  [map_snps_to_genes](https://neurogenomics.github.io/MAGMA_Celltyping/reference/map_snps_to_genes.md)
  function).

- ctd:

  Cell type data structure. Must contain quantiles.

- annotLevel:

  Annot level for which the gene covar file should be constructed

- ctd_species:

  Species name relevant to the cell type data, i.e. "mouse" or "human"

- genesOutCOND:

  \[Optional\] Path to a *genes.out* file to condition on. Used if you
  want to condition on a different GWAS.

- verbose:

  Print messages.

## Value

File path for the gene covar file.
