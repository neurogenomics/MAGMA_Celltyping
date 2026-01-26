# Get CTD dendro

Get all dendrogram features used for plotting a dendrogram of cell
types.

## Usage

``` r
get_ctd_dendro(ctd, annotLevel, expand = c(0, 0.66), verbose = TRUE, ...)
```

## Source

` MAGMA.Celltyping:::get_ctd_dendro(ctd = ctd, annotLevel = 1) `

## Arguments

- ctd:

  Cell type data structure containing `specificity_quantiles`.

- annotLevel:

  Annot level for which the gene covar file should be constructed.

- expand:

  For position scales, a vector of range expansion constants used to add
  some padding around the data to ensure that they are placed some
  distance away from the axes. Use the convenience function
  [`expansion()`](https://ggplot2.tidyverse.org/reference/expansion.html)
  to generate the values for the `expand` argument. The defaults are to
  expand the scale by 5% on each side for continuous variables, and by
  0.6 units on each side for discrete variables.

- verbose:

  Print messages.

- ...:

  Arguments passed on to
  [`prepare_quantile_groups`](https://neurogenomics.github.io/MAGMA_Celltyping/reference/prepare_quantile_groups.md)

  `standardise`

  :   Whether to run
      [standardise_ctd](https://rdrr.io/pkg/EWCE/man/standardise_ctd.html)
      first. Provides gene ortholog conversion.

  `input_species`

  :   Which species the gene names in `exp` come from. See
      [list_species](https://rdrr.io/pkg/EWCE/man/list_species.html) for
      all available species.

  `output_species`

  :   Which species' genes names to convert `exp` to. See
      [list_species](https://rdrr.io/pkg/EWCE/man/list_species.html) for
      all available species.

  `non121_strategy`

  :   How to handle genes that don't have 1:1 mappings between
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

      :   Keep all genes regardless of whether they have duplicate
          mappings in either species.

      `"keep_popular" or "kp" or 5`

      :   Return only the most "popular" interspecies ortholog mappings.
          This procedure tends to yield a greater number of returned
          genes but at the cost of many of them not being true
          biological 1:1 orthologs.

      `"sum","mean","median","min" or "max"`

      :   When `gene_df` is a matrix and `gene_output="rownames"`, these
          options will aggregate many-to-one gene mappings
          (`input_species`-to-`output_species`) after dropping any
          duplicate genes in the `output_species`.

  `numberOfBins`

  :   Number of non-zero quantile bins.

## Value

List containing ddata and ordered_cells
