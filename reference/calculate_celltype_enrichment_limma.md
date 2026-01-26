# Calculate celltype enrichment

Calculate celltype enrichments without MAGMA using adjusted MAGMA
Z-statistic from .genes.out files.

## Usage

``` r
calculate_celltype_enrichment_limma(
  magmaAdjZ,
  ctd,
  ctd_species = infer_ctd_species(ctd = ctd),
  annotLevel = 1,
  prepare_ctd = TRUE,
  thresh = 1e-04,
  celltypes = NULL,
  return_all = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- magmaAdjZ:

  Output from
  [adjust_zstat_in_genesOut](https://neurogenomics.github.io/MAGMA_Celltyping/reference/adjust_zstat_in_genesOut.md).

- ctd:

  Cell type data structure containing `specificity_quantiles`.

- ctd_species:

  Species name relevant to the CellTypeDataset (`ctd`). See
  [list_species](https://rdrr.io/pkg/EWCE/man/list_species.html) for all
  available species. If `ctd_species=NULL` (default), the `ctd` species
  will automatically be inferred using
  [infer_species](https://rdrr.io/pkg/orthogene/man/infer_species.html).

- annotLevel:

  Level within the `ctd` to use.

- prepare_ctd:

  Whether to run
  [prepare_quantile_groups](https://neurogenomics.github.io/MAGMA_Celltyping/reference/prepare_quantile_groups.md)
  on the `ctd` first.

- thresh:

  A threshold on low specificity values (used to drop genes).

- celltypes:

  Cell types to conduct tests for. Defaults to all cell types available
  in selected `ctd` level.

- return_all:

  Return a list of both the processed data input and the enrichment
  results (Default). If `FALSE`, will only return enrichment results.

- verbose:

  Print messages.

- ...:

  Additional arguments passed to
  [standardise_ctd](https://rdrr.io/pkg/EWCE/man/standardise_ctd.html).

## Examples

``` r
ctd <- ewceData::ctd()
#> see ?ewceData and browseVignettes('ewceData') for documentation
#> loading from cache

# The package stores an example genesOut file, so save this to a tempfile
myGenesOut <- MAGMA.Celltyping::import_magma_files(
    ids = c("ieu-a-298"),
    file_types = ".genes.out",
    return_dir = FALSE)
#> Using built-in example files: ieu-a-298.tsv.gz.35UP.10DOWN
#> Returning MAGMA gene.* file paths
    
#### Get adjust gene z-scores ####    
magmaAdjZ <- MAGMA.Celltyping::adjust_zstat_in_genesOut(
    ctd = ctd,
    magma_GenesOut_file = myGenesOut)
#> ctd_species=NULL: Inferring species from gene names.
#> Preparing gene_df.
#> Dense matrix format detected.
#> Extracting genes from rownames.
#> 15,259 genes extracted.
#> Testing for gene overlap with: human
#> Retrieving all genes using: homologene.
#> Retrieving all organisms available in homologene.
#> Mapping species name: human
#> Common name mapping found for human
#> 1 organism identified from search: 9606
#> Using cached file: /github/home/.cache/R/orthogene/all_genes-9606-homologene.csv.gz
#> Returning all 19,129 genes from human.
#> Testing for gene overlap with: monkey
#> Retrieving all genes using: homologene.
#> Retrieving all organisms available in homologene.
#> Mapping species name: monkey
#> Common name mapping found for monkey
#> 1 organism identified from search: 9544
#> Using cached file: /github/home/.cache/R/orthogene/all_genes-9544-homologene.csv.gz
#> Returning all 16,843 genes from monkey.
#> Testing for gene overlap with: rat
#> Retrieving all genes using: homologene.
#> Retrieving all organisms available in homologene.
#> Mapping species name: rat
#> Common name mapping found for rat
#> 1 organism identified from search: 10116
#> Using cached file: /github/home/.cache/R/orthogene/all_genes-10116-homologene.csv.gz
#> Returning all 20,616 genes from rat.
#> Testing for gene overlap with: mouse
#> Retrieving all genes using: homologene.
#> Retrieving all organisms available in homologene.
#> Mapping species name: mouse
#> Common name mapping found for mouse
#> 1 organism identified from search: 10090
#> Using cached file: /github/home/.cache/R/orthogene/all_genes-10090-homologene.csv.gz
#> Returning all 21,207 genes from mouse.
#> Testing for gene overlap with: zebrafish
#> Retrieving all genes using: homologene.
#> Retrieving all organisms available in homologene.
#> Mapping species name: zebrafish
#> Common name mapping found for zebrafish
#> 1 organism identified from search: 7955
#> Using cached file: /github/home/.cache/R/orthogene/all_genes-7955-homologene.csv.gz
#> Returning all 20,897 genes from zebrafish.
#> Testing for gene overlap with: fly
#> Retrieving all genes using: homologene.
#> Retrieving all organisms available in homologene.
#> Mapping species name: fly
#> Common name mapping found for fly
#> 1 organism identified from search: 7227
#> Using cached file: /github/home/.cache/R/orthogene/all_genes-7227-homologene.csv.gz
#> Returning all 8,438 genes from fly.
#> Top match:
#>   - species: mouse 
#>   - percent_match: 92%
#> Inferred ctd species: mouse
#> Standardising CellTypeDataset
#> Found 5 matrix types across 2 CTD levels.
#> Processing level: 1
#> Processing level: 2
#> Importing genes.out file.
#> 4 genes without HGNC gene symbols were dropped.
#> 371 genes that are absent from the ctd were dropped.
#> Computing adjusted Z-statistic.
    
#### Get cell type enrichment ####
ps <- MAGMA.Celltyping::calculate_celltype_enrichment_limma(
    magmaAdjZ = magmaAdjZ,
    ctd = ctd)
#> ctd_species=NULL: Inferring species from gene names.
#> Preparing gene_df.
#> Dense matrix format detected.
#> Extracting genes from rownames.
#> 15,259 genes extracted.
#> Testing for gene overlap with: human
#> Retrieving all genes using: homologene.
#> Retrieving all organisms available in homologene.
#> Mapping species name: human
#> Common name mapping found for human
#> 1 organism identified from search: 9606
#> Using cached file: /github/home/.cache/R/orthogene/all_genes-9606-homologene.csv.gz
#> Returning all 19,129 genes from human.
#> Testing for gene overlap with: monkey
#> Retrieving all genes using: homologene.
#> Retrieving all organisms available in homologene.
#> Mapping species name: monkey
#> Common name mapping found for monkey
#> 1 organism identified from search: 9544
#> Using cached file: /github/home/.cache/R/orthogene/all_genes-9544-homologene.csv.gz
#> Returning all 16,843 genes from monkey.
#> Testing for gene overlap with: rat
#> Retrieving all genes using: homologene.
#> Retrieving all organisms available in homologene.
#> Mapping species name: rat
#> Common name mapping found for rat
#> 1 organism identified from search: 10116
#> Using cached file: /github/home/.cache/R/orthogene/all_genes-10116-homologene.csv.gz
#> Returning all 20,616 genes from rat.
#> Testing for gene overlap with: mouse
#> Retrieving all genes using: homologene.
#> Retrieving all organisms available in homologene.
#> Mapping species name: mouse
#> Common name mapping found for mouse
#> 1 organism identified from search: 10090
#> Using cached file: /github/home/.cache/R/orthogene/all_genes-10090-homologene.csv.gz
#> Returning all 21,207 genes from mouse.
#> Testing for gene overlap with: zebrafish
#> Retrieving all genes using: homologene.
#> Retrieving all organisms available in homologene.
#> Mapping species name: zebrafish
#> Common name mapping found for zebrafish
#> 1 organism identified from search: 7955
#> Using cached file: /github/home/.cache/R/orthogene/all_genes-7955-homologene.csv.gz
#> Returning all 20,897 genes from zebrafish.
#> Testing for gene overlap with: fly
#> Retrieving all genes using: homologene.
#> Retrieving all organisms available in homologene.
#> Mapping species name: fly
#> Common name mapping found for fly
#> 1 organism identified from search: 7227
#> Using cached file: /github/home/.cache/R/orthogene/all_genes-7227-homologene.csv.gz
#> Returning all 8,438 genes from fly.
#> Top match:
#>   - species: mouse 
#>   - percent_match: 92%
#> Inferred ctd species: mouse
#> Standardising CellTypeDataset
#> Found 5 matrix types across 2 CTD levels.
#> Processing level: 1
#> Processing level: 2
#> Preparing specificity matrix.
#> Running enrichment tests: astrocytes_ependymal
#> Removing intercept from test coefficients
#> Running enrichment tests: endothelial_mural
#> Removing intercept from test coefficients
#> Running enrichment tests: interneurons
#> Removing intercept from test coefficients
#> Running enrichment tests: microglia
#> Removing intercept from test coefficients
#> Running enrichment tests: oligodendrocytes
#> Removing intercept from test coefficients
#> Running enrichment tests: pyramidal_CA1
#> Removing intercept from test coefficients
#> Running enrichment tests: pyramidal_SS
#> Removing intercept from test coefficients
```
