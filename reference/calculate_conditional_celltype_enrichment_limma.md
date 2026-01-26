# Calculate celltype enrichments without MAGMA

Calculate celltype enrichments without MAGMA using adjusted MAGMA
Z-statistic from .genes.out files.

## Usage

``` r
calculate_conditional_celltype_enrichment_limma(
  magma1,
  magma2,
  ctd,
  ctd_species = infer_ctd_species(ctd),
  annotLevel = 1,
  prepare_ctd = TRUE,
  thresh = 1e-04,
  show_plot = TRUE,
  verbose = TRUE,
  ...
)
```

## Arguments

- magma1:

  Output from
  [adjust_zstat_in_genesOut](https://neurogenomics.github.io/MAGMA_Celltyping/reference/adjust_zstat_in_genesOut.md)
  (e.g. trait 1).

- magma2:

  Output from
  [adjust_zstat_in_genesOut](https://neurogenomics.github.io/MAGMA_Celltyping/reference/adjust_zstat_in_genesOut.md)
  (e.g. trait 2).

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

- show_plot:

  Show plot visualizations of results (Default: `TRUE`).

- verbose:

  Print messages.

- ...:

  Additional arguments passed to
  [standardise_ctd](https://rdrr.io/pkg/EWCE/man/standardise_ctd.html).

## Value

Data.frame with conditional enrichment results.

## Examples

``` r
ctd <- ewceData::ctd()
#> see ?ewceData and browseVignettes('ewceData') for documentation
#> loading from cache

#### MAGMA 1 #####
genesOut1 <- MAGMA.Celltyping::import_magma_files(
    ids = c("ieu-a-298"),
    file_types = ".genes.out",
    return_dir = FALSE)
#> Using built-in example files: ieu-a-298.tsv.gz.35UP.10DOWN
#> Returning MAGMA gene.* file paths
magma1 <- MAGMA.Celltyping::adjust_zstat_in_genesOut(
    ctd = ctd, 
    magma_GenesOut_file = genesOut1)
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

#### MAGMA 2 #####
genesOut2 <- MAGMA.Celltyping::import_magma_files(
    ids = c("ukb-a-333"),
    file_types = ".genes.out",
    return_dir = FALSE)
#> Filtering IDs to only 1 requested dataset(s).
#> Downloading 1 files.
#> Returning MAGMA gene.* file paths
magma2 <- MAGMA.Celltyping::adjust_zstat_in_genesOut(
    ctd = ctd, 
    magma_GenesOut_file = genesOut2)
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
#> 55 genes without HGNC gene symbols were dropped.
#> 5,799 genes that are absent from the ctd were dropped.
#> 13 genes with duplicated HGNC gene symbols were dropped.
#> Computing adjusted Z-statistic.

#### Run conditional enrichment ####
cond_res <- 
MAGMA.Celltyping::calculate_conditional_celltype_enrichment_limma(
    magma1 = magma1,
    magma2 = magma2,
    ctd = ctd)
#> Correlation between magma1 and magma2 ADJ_ZSTAT: 0.051
#> 
#> Call:
#> stats::lm(formula = ADJ_ZSTAT.x ~ ADJ_ZSTAT.y, data = m3)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -4.3930 -0.5094  0.0290  0.5759  3.1231 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  5.29185    0.02921 181.141   <2e-16 ***
#> ADJ_ZSTAT.y  0.03827    0.02433   1.573    0.116    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 0.9046 on 960 degrees of freedom
#> Multiple R-squared:  0.002571,   Adjusted R-squared:  0.001532 
#> F-statistic: 2.474 on 1 and 960 DF,  p-value: 0.1161
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
#> Running conditonal enrichment tests: astrocytes_ependymal
#> Running conditonal enrichment tests: endothelial_mural
#> Running conditonal enrichment tests: interneurons
#> Running conditonal enrichment tests: microglia
#> Running conditonal enrichment tests: oligodendrocytes
#> Running conditonal enrichment tests: pyramidal_CA1
#> Running conditonal enrichment tests: pyramidal_SS
#> ctd is already standardised. Returning original ctd.
#>  Set force_standardise=TRUE to re-standardise.
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
#> ctd is already standardised. Returning original ctd.
#>  Set force_standardise=TRUE to re-standardise.
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
