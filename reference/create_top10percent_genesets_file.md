# Create gene covar file

The gene covar file is the input to MAGMA for the celltype association
analysis. This code was functonalised because it is called by both
baseline and conditional analysis.

## Usage

``` r
create_top10percent_genesets_file(
  genesOutFile,
  ctd,
  annotLevel,
  ctd_species,
  verbose = TRUE
)
```

## Source

` #### Prepare cell-type data #### ctd <- ewceData::ctd()#### Prepare GWAS MAGMA data #### myGenesOut <- MAGMA.Celltyping::import_magma_files(ids = "ieu-a-298", file_types = ".genes.out", return_dir = FALSE) ctd <- MAGMA.Celltyping::prepare_quantile_groups(ctd = ctd) geneSetsFilePath <- MAGMA.Celltyping:::create_top10percent_genesets_file( genesOutFile = myGenesOut, ctd = ctd, annotLevel = 1, ## Mapped to human orths by prepare_quantile_groups previously ctd_species = "human" ) `

## Arguments

- genesOutFile:

  The output of the second call to MAGMA (performed in the
  map_snps_to_genes function).

- ctd:

  Cell type data structure containing `specificity_quantiles`.

- ctd_species:

  Species name relevant to the CellTypeDataset (`ctd`). See
  [list_species](https://rdrr.io/pkg/EWCE/man/list_species.html) for all
  available species. If `ctd_species=NULL` (default), the `ctd` species
  will automatically be inferred using
  [infer_species](https://rdrr.io/pkg/orthogene/man/infer_species.html).

- verbose:

  Print messages.

## Value

Filepath for the gene covar file
