# Calculate conditional celltype associations using MAGMA

Run cell-type enrichment analysis on a GWAS previously mapped to genes
(using
[map_snps_to_genes](https://neurogenomics.github.io/MAGMA_Celltyping/reference/map_snps_to_genes.md))
while controlling for certain cell-types. This allows one to conduct
cell-type enrichment analyses while controlling for the strongest
cell-type-specific signatures. Which cell-types are controlled for can
be specified by either of the following arguments:

- `controlTopNcells`Automatically selects the top N mostly significantly
  enriched cell-types.

- `controlledCTs`A user-provided list of cell-types present in the
  `ctd`.

Three sets of analyses are run:

- Baseline enrichment resultsNo conditioning.

- Conditional results: separate conditioning on each specified cell-type
  separately

- Conditional results: grouped conditioning all specified cell-types at
  once.

## Usage

``` r
calculate_conditional_celltype_associations(
  ctd,
  ctd_species = infer_ctd_species(ctd),
  gwas_sumstats_path = NULL,
  magma_dir = NULL,
  analysis_name = "MainRun",
  prepare_ctd = TRUE,
  upstream_kb = 35,
  downstream_kb = 10,
  controlledAnnotLevel = 1,
  controlTopNcells = NA,
  controlledCTs = NA,
  EnrichmentMode = "Linear",
  qvalue_thresh = 0.05,
  force_new = FALSE,
  version = NULL,
  verbose = TRUE
)
```

## Arguments

- ctd:

  Cell type data structure containing `specificity_quantiles`.

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

- controlledAnnotLevel:

  Which annotation level should be controlled for.

- controlTopNcells:

  How many of the most significant cell types at that annotation level
  should be controlled for?

- controlledCTs:

  Array of the celltype to be controlled for, e.g.
  `c('Interneuron type 16','Medium Spiny Neuron')`.

- EnrichmentMode:

  \[Optional\] Should either 'Linear' or 'Top 10%' mode be used for
  testing enrichment?

- qvalue_thresh:

  Multiple-testing corrected p-value threshold to filter by when
  determining which celltypes to condition with.

- force_new:

  \[Optional\] Force new MAGMA analyses even if the pre-existing results
  files are detected.

- version:

  MAGMA version to use.

- verbose:

  Print messages.

## Value

A concatenated results table containing:

- Baseline enrichment results.

- Conditional results: conditioning on each specified cell-type
  individually.

- Conditional results: conditioning all specified cell-types at once.

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
ctAssocs <- calculate_conditional_celltype_associations(
    ctd = ctd,
    controlledAnnotLevel = 1,
    controlTopNcells = 1,
    qvalue_thresh = 1,
    magma_dir = magma_dir,
    ctd_species = "mouse", 
    force = TRUE) 
#> WARNING: Setting qvalue_thresh>0.05 is not reccommended in practice.
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
#> Installed MAGMA version: v1.10
#> Skipping MAGMA installation.
#> The desired_version of MAGMA is currently installed: v1.10
#> Using: magma_v1.10
#> ctd is already standardised. Returning original ctd.
#>  Set force_standardise=TRUE to re-standardise.
#> Converting to sparse matrix.
#> Converting to sparse matrix.
#> Running MAGMA: Linear mode
#> Mapping gene symbols in specificity_quantiles matrix to entrez IDs.
#> Reading enrichment results file into R.
#> Running MAGMA: Linear mode
#> Mapping gene symbols in specificity_quantiles matrix to entrez IDs.
#> Reading enrichment results file into R.
#> Mapping gene symbols in specificity_quantiles matrix to entrez IDs.
#> Mapping gene symbols in specificity_quantiles matrix to entrez IDs.
#> magma  
#>   --gene-results '/tmp/RtmpEkCLL9/MAGMA_Files/ieu-a-298.tsv.gz.35UP.10DOWN/ieu-a-298.tsv.gz.35UP.10DOWN.genes.raw' 
#>   --gene-covar '/tmp/RtmpEkCLL9/file44fe24e77466' 
#>   --model direction=pos condition='oligodendrocytes' 
#>   --out '/tmp/RtmpEkCLL9/MAGMA_Files/ieu-a-298.tsv.gz.35UP.10DOWN/ieu-a-298.tsv.gz.35UP.10DOWN.level1.35UP.10DOWN.Linear.ControlFor_oligodendrocytes'
#> Reading enrichment results file into R.
#> magma  
#>   --gene-results '/tmp/RtmpEkCLL9/MAGMA_Files/ieu-a-298.tsv.gz.35UP.10DOWN/ieu-a-298.tsv.gz.35UP.10DOWN.genes.raw' 
#>   --gene-covar '/tmp/RtmpEkCLL9/file44fe24e77466' 
#>   --model direction=pos condition='oligodendrocytes' 
#>   --out '/tmp/RtmpEkCLL9/MAGMA_Files/ieu-a-298.tsv.gz.35UP.10DOWN/ieu-a-298.tsv.gz.35UP.10DOWN.level1.35UP.10DOWN.ControlFor_oligodendrocytes'
#> Reading enrichment results file into R.
#> Mapping gene symbols in specificity_quantiles matrix to entrez IDs.
#> magma  
#>   --gene-results '/tmp/RtmpEkCLL9/MAGMA_Files/ieu-a-298.tsv.gz.35UP.10DOWN/ieu-a-298.tsv.gz.35UP.10DOWN.genes.raw' 
#>   --gene-covar '/tmp/RtmpEkCLL9/file44fe32b513fc' 
#>   --model direction=pos condition='oligodendrocytes' 
#>   --out '/tmp/RtmpEkCLL9/MAGMA_Files/ieu-a-298.tsv.gz.35UP.10DOWN/ieu-a-298.tsv.gz.35UP.10DOWN.level2.35UP.10DOWN.Linear.ControlFor_oligodendrocytes'
#> Reading enrichment results file into R.
#> magma  
#>   --gene-results '/tmp/RtmpEkCLL9/MAGMA_Files/ieu-a-298.tsv.gz.35UP.10DOWN/ieu-a-298.tsv.gz.35UP.10DOWN.genes.raw' 
#>   --gene-covar '/tmp/RtmpEkCLL9/file44fe32b513fc' 
#>   --model direction=pos condition='oligodendrocytes' 
#>   --out '/tmp/RtmpEkCLL9/MAGMA_Files/ieu-a-298.tsv.gz.35UP.10DOWN/ieu-a-298.tsv.gz.35UP.10DOWN.level2.35UP.10DOWN.ControlFor_oligodendrocytes'
#> Reading enrichment results file into R.
```
