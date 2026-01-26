# Calculate cell type associations using MAGMA

Has the option of running multiple analyses with a single function.
Assumes that you already have all MAGMA GWAS files precomputed.
Precomputed MAGMA GWAS files can be downloaded via the
[import_magma_files](https://neurogenomics.github.io/MAGMA_Celltyping/reference/import_magma_files.md)
function.

## Usage

``` r
celltype_associations_pipeline(
  ctd,
  ctd_levels = seq_len(length(ctd)),
  ctd_name,
  ctd_species = infer_ctd_species(ctd),
  standardise = TRUE,
  magma_dirs,
  run_linear = TRUE,
  run_top10 = TRUE,
  run_conditional = FALSE,
  upstream_kb = 35,
  downstream_kb = 10,
  suffix_linear = "linear",
  suffix_top10 = "top10",
  suffix_condition = "condition",
  controlledAnnotLevel = 1,
  controlTopNcells = 1,
  force_new = FALSE,
  save_dir = tempdir(),
  nThread = 1,
  version = NULL,
  verbose = TRUE
)
```

## Arguments

- ctd:

  Cell type data structure containing `specificity_quantiles`.

- ctd_levels:

  Which levels of `ctd` to iterate the enrichment analysis over.

- ctd_name:

  Used in file names

- ctd_species:

  Species name relevant to the CellTypeDataset (`ctd`). See
  [list_species](https://rdrr.io/pkg/EWCE/man/list_species.html) for all
  available species. If `ctd_species=NULL` (default), the `ctd` species
  will automatically be inferred using
  [infer_species](https://rdrr.io/pkg/orthogene/man/infer_species.html).

- standardise:

  Whether to run
  [standardise_ctd](https://rdrr.io/pkg/EWCE/man/standardise_ctd.html)
  first. Provides gene ortholog conversion.

- magma_dirs:

  Path to folders containing the pre-computed MAGMA GWAS files
  (*.gsa.raw*and *.gsa.out*). *NOTE*: Files within these folders must
  have the same naming scheme as the folders themselves.

- run_linear:

  Run in linear mode.

- run_top10:

  Run in top 10% mode.

- run_conditional:

  Run in conditional mode.

- upstream_kb:

  How many kb upstream of the gene should SNPs be included?

- downstream_kb:

  How many kb downstream of the gene should SNPs be included?

- suffix_linear:

  This will be added to the linear results file name.

- suffix_top10:

  This will be added to the top 10% results file name.

- suffix_condition:

  This will be added to the conditional results file name.

- controlledAnnotLevel:

  Which annotation level should be controlled for.

- controlTopNcells:

  How many of the most significant cell types at that annotation level
  should be controlled for?

- force_new:

  \[Optional\] Force new MAGMA analyses even if the pre-existing results
  files are detected.

- save_dir:

  Folder to save results in (`save_dir=NULL` to not save any results).

- nThread:

  Number of threads to parallelise analyses across.

- version:

  MAGMA version to use.

- verbose:

  Print messages.

## Value

A list containing the results of each selected celltype associations
analysis.

## Examples

``` r
magma_dirs <- MAGMA.Celltyping::import_magma_files(ids = c("ieu-a-298"))
#> Using built-in example files: ieu-a-298.tsv.gz.35UP.10DOWN
#> Returning MAGMA directories.
ctd <- ewceData::ctd()
#> see ?ewceData and browseVignettes('ewceData') for documentation
#> loading from cache

res <- MAGMA.Celltyping::celltype_associations_pipeline(
    ctd = ctd,
    ctd_levels = 1,
    ctd_name = "Zeisel2015",
    ctd_species = "mouse",
    magma_dirs = magma_dirs)
#> Preparing CellTypeDataset.
#> Converting to sparse matrix.
#> Converting to sparse matrix.
#> ieu-a-298.tsv.gz.35UP.10DOWN
#> ======= Calculating celltype associations: linear mode =======
#> Installed MAGMA version: v1.10
#> Skipping MAGMA installation.
#> The desired_version of MAGMA is currently installed: v1.10
#> Using: magma_v1.10
#> Running MAGMA: Linear mode
#> Mapping gene symbols in specificity_quantiles matrix to entrez IDs.
#> Reading enrichment results file into R.
#> ======= Calculating celltype associations: top10% mode =======
#> Installed MAGMA version: v1.10
#> Skipping MAGMA installation.
#> The desired_version of MAGMA is currently installed: v1.10
#> Using: magma_v1.10
#> Running MAGMA: Top 10% mode
#> Mapping gene symbols in specificity_deciles matrix to entrez IDs.
#> Constructing top10% gene marker sets for 8 cell-types.
#> Reading enrichment results file into R.
#> Merging linear and top10% results
#> Saving results ==> /tmp/RtmpEkCLL9/Zeisel2015/MAGMA_celltyping.Zeisel2015.rds
```
