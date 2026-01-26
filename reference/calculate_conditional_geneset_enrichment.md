# Use MAGMA to test enrichment in a geneset

Run gene set enrichment analysis with a given `geneset` on a GWAS
previously mapped to genes (using
[map_snps_to_genes](https://neurogenomics.github.io/MAGMA_Celltyping/reference/map_snps_to_genes.md)),
while conditioning on a given cell-type or list of cell-types
(`controlledCTs`), as defined by the "specificity_quantiles" matrix in a
given level (`controlledAnnotLevel`) of the provide CellTypeDataset
(`ctd`). This allows one to conduct gene set enrichment analyses while
controlling for strong cell-type-specific signatures.

## Usage

``` r
calculate_conditional_geneset_enrichment(
  geneset,
  ctd,
  ctd_species = infer_ctd_species(ctd),
  controlledAnnotLevel = 1,
  controlledCTs,
  prepare_ctd = TRUE,
  gwas_sumstats_path = NULL,
  magma_dir = NULL,
  analysis_name = "MainRun",
  upstream_kb = 35,
  downstream_kb = 10,
  geneset_species = infer_geneset_species(geneset),
  version = NULL,
  verbose = TRUE
)
```

## Arguments

- geneset:

  Genes which are to be tested. Can be gene symbols (or other gene IDs)
  from any species listed in
  [list_species](https://rdrr.io/pkg/EWCE/man/list_species.html).

- ctd:

  Cell type data structure containing `specificity_quantiles`.

- ctd_species:

  Species name relevant to the CellTypeDataset (`ctd`). See
  [list_species](https://rdrr.io/pkg/EWCE/man/list_species.html) for all
  available species. If `ctd_species=NULL` (default), the `ctd` species
  will automatically be inferred using
  [infer_species](https://rdrr.io/pkg/orthogene/man/infer_species.html).

- controlledAnnotLevel:

  Annotation level of the celltypes being controlled for.

- controlledCTs:

  Array of the celltype to be controlled for, e.g. c("Interneuron type
  16","Medium Spiny Neuron).

- prepare_ctd:

  Whether to run
  [prepare_quantile_groups](https://neurogenomics.github.io/MAGMA_Celltyping/reference/prepare_quantile_groups.md)
  on the `ctd` first.

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

[data.table](https://rdrr.io/pkg/data.table/man/data.table.html) of
baseline (column name prefix "BASELINE\_") and conditioned (column name
prefix "COND\_") gene set enrichment results.

## Examples

``` r
#### Import data ####
ctd <- MAGMA.Celltyping::get_ctd("ctd_allKI")
#> Loading required namespace: piggyback
#> Downloading 1 CellTypeDataset file(s).
#> ℹ All local files already up-to-date!
magma_dir <- MAGMA.Celltyping::import_magma_files(ids = "ieu-a-298")
#> Using built-in example files: ieu-a-298.tsv.gz.35UP.10DOWN
#> Returning MAGMA directories.
geneset <- MAGMA.Celltyping::rbfox_binding

res <- MAGMA.Celltyping::calculate_conditional_geneset_enrichment(
    geneset = geneset,
    ctd = ctd,
    controlledAnnotLevel = 1,
    controlledCTs = "pyramidal SS",
    magma_dir = magma_dir,
    analysis_name = "Rbfox_16_pyrSS",  
    geneset_species = "mouse", 
    ctd_species = "mouse") 
#> Installed MAGMA version: v1.10
#> Skipping MAGMA installation.
#> The desired_version of MAGMA is currently installed: v1.10
#> Using: magma_v1.10
#> ctd is already standardised. Returning original ctd.
#>  Set force_standardise=TRUE to re-standardise.
#> Converting to sparse matrix.
#> Converting to sparse matrix.
#> Preparing gene_df.
#> character format detected.
#> Converting to data.frame
#> Extracting genes from input_gene.
#> 4,143 genes extracted.
#> Converting mouse ==> human orthologs using: homologene
#> Retrieving all organisms available in homologene.
#> Mapping species name: mouse
#> Common name mapping found for mouse
#> 1 organism identified from search: 10090
#> Retrieving all organisms available in homologene.
#> Mapping species name: human
#> Common name mapping found for human
#> 1 organism identified from search: 9606
#> Checking for genes without orthologs in human.
#> Extracting genes from input_gene.
#> 3,769 genes extracted.
#> Extracting genes from ortholog_gene.
#> 3,769 genes extracted.
#> Checking for genes without 1:1 orthologs.
#> Dropping 13 genes that have multiple input_gene per ortholog_gene (many:1).
#> Dropping 2 genes that have multiple ortholog_gene per input_gene (1:many).
#> Filtering gene_df with gene_map
#> Adding input_gene col to gene_df.
#> Adding ortholog_gene col to gene_df.
#> 
#> =========== REPORT SUMMARY ===========
#> Total genes dropped after convert_orthologs :
#>    398 / 4,143 (9.6%)
#> Total genes remaining after convert_orthologs :
#>    3,745 / 4,143 (90%)
#> Mapping gene symbols in specificity_quantiles matrix to entrez IDs.
#> Reading sets out info from .gsa.out file.
```
