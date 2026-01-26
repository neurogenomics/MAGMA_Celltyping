# Prepare quantile groups for each celltype based on specificity

Quantile groups are stored in an extra matrix ('quantiles') in the
returned CTD. This function also removes any genes from the CTD data
which are not 1:1 orthologs with the GWAS species.

## Usage

``` r
prepare_quantile_groups(
  ctd,
  standardise = TRUE,
  non121_strategy = "drop_both_species",
  input_species = "mouse",
  output_species = "human",
  numberOfBins = 40,
  verbose = TRUE,
  ...
)
```

## Arguments

- ctd:

  Input CellTypeData.

- standardise:

  Whether to run
  [standardise_ctd](https://rdrr.io/pkg/EWCE/man/standardise_ctd.html)
  first. Provides gene ortholog conversion.

- non121_strategy:

  How to handle genes that don't have 1:1 mappings between
  `input_species`:`output_species`. Options include:  

  `"drop_both_species" or "dbs" or 1`

  :   Drop genes that have duplicate mappings in either the
      `input_species` or `output_species` (*DEFAULT*).

  `"drop_input_species" or "dis" or 2`

  :   Only drop genes that have duplicate mappings in the
      `input_species`.

  `"drop_output_species" or "dos" or 3`

  :   Only drop genes that have duplicate mappings in the
      `output_species`.

  `"keep_both_species" or "kbs" or 4`

  :   Keep all genes regardless of whether they have duplicate mappings
      in either species.

  `"keep_popular" or "kp" or 5`

  :   Return only the most "popular" interspecies ortholog mappings.
      This procedure tends to yield a greater number of returned genes
      but at the cost of many of them not being true biological 1:1
      orthologs.

  `"sum","mean","median","min" or "max"`

  :   When `gene_df` is a matrix and `gene_output="rownames"`, these
      options will aggregate many-to-one gene mappings
      (`input_species`-to-`output_species`) after dropping any duplicate
      genes in the `output_species`.

- input_species:

  Which species the gene names in `exp` come from. See
  [list_species](https://rdrr.io/pkg/EWCE/man/list_species.html) for all
  available species.

- output_species:

  Which species' genes names to convert `exp` to. See
  [list_species](https://rdrr.io/pkg/EWCE/man/list_species.html) for all
  available species.

- numberOfBins:

  Number of non-zero quantile bins.

- verbose:

  Print messages. Set `verbose=2` if you want to print all messages from
  internal functions as well.

- ...:

  Additional arguments passed to
  [standardise_ctd](https://rdrr.io/pkg/EWCE/man/standardise_ctd.html).

## Value

The ctd converted to `output_species` gene symbols with additional
quantiles matrix.

## Examples

``` r
ctd_orig <- ewceData::ctd()
#> see ?ewceData and browseVignettes('ewceData') for documentation
#> loading from cache
ctd <- MAGMA.Celltyping::prepare_quantile_groups(ctd = ctd_orig)
#> Standardising CellTypeDataset
#> Found 5 matrix types across 2 CTD levels.
#> Processing level: 1
#> Processing level: 2
#> Converting to sparse matrix.
#> Converting to sparse matrix.
```
