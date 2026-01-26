# Load a MAGMA .gcov.out file

Convenience function which just does a little formatting to make it
easier to use.

## Usage

``` r
load_magma_results_file(
  path,
  ctd,
  annotLevel,
  EnrichmentMode,
  genesOutCOND = NA,
  analysis_name = NULL,
  ControlForCT = "BASELINE",
  keep_all_rows = FALSE,
  verbose = TRUE
)
```

## Arguments

- path:

  Path to a .gcov.out file (if `EnrichmentMode=='Linear'`). or .sets.out
  (if EnrichmentMode=='Top 10%').

- ctd:

  Cell type data structure containing `specificity_quantiles`.

- annotLevel:

  Level within the `ctd` to use.

- EnrichmentMode:

  \[Optional\] Either 'Linear' or 'Top 10%' (Default: `'Linear'`).

- genesOutCOND:

  \[Optional\] If the analysis controlled for another GWAS, then this is
  included as a column. Otherwise the column is `NA`.

- analysis_name:

  Used in file names which area created.

- ControlForCT:

  \[Optional\] May be an internal argument. We suggest ignoring or take
  a look at the code to figure it out.

- keep_all_rows:

  Keep all rows, without filtering such that there is only one row per
  celltype.

- verbose:

  Print messages.

## Value

Contents of the .gcov.out file.

## Examples

``` r
path <- system.file(
    "extdata","ieu-a-298.tsv.gz.35UP.10DOWN.level1.MainRun.gsa.out",
    package = "MAGMA.Celltyping")
ctd <- ewceData::ctd()
#> see ?ewceData and browseVignettes('ewceData') for documentation
#> loading from cache
annotLevel <- 1
EnrichmentMode <- "Linear"
res <- load_magma_results_file(path = path, 
                               ctd = ctd,
                               annotLevel = annotLevel,
                               EnrichmentMode = EnrichmentMode)
#> Reading enrichment results file into R.
```
