# MAGMA tile plot

Used after merging results from multiple GWAS studies.

## Usage

``` r
magma_tileplot(
  ctd,
  results,
  height = 13,
  width = 4,
  annotLevel = 1,
  fill = "-log10p",
  fileTag = "",
  output_path = tempdir(),
  qvalue_thresh = NULL,
  plotDendro = TRUE,
  show_plot = TRUE,
  bind_plots = TRUE,
  wrap_width = 50,
  verbose = TRUE
)
```

## Arguments

- ctd:

  Cell type data structure containing "specificity_quantiles".

- results:

  Output from either
  [calculate_celltype_associations](https://neurogenomics.github.io/MAGMA_Celltyping/reference/calculate_celltype_associations.md)
  or
  [calculate_conditional_celltype_associations](https://neurogenomics.github.io/MAGMA_Celltyping/reference/calculate_conditional_celltype_associations.md).

- height:

  Height of the output tileplot.

- width:

  Width of the output tileplot.

- annotLevel:

  Annotation level to plot the results for.

- fill:

  Variable in `results` to fill the plot tile colors by. Passed to
  [aes](https://ggplot2.tidyverse.org/reference/aes.html).

- fileTag:

  String appended to the names of the saved PDFs, i.e. the name of the
  celltype data file used.

- output_path:

  Location where the results should be plotted.

- qvalue_thresh:

  The multiple-testing corrected p-value at which to filter results (set
  to `NULL` to skip filtering).

- plotDendro:

  Should the dendrogram of celltypes be shown alongside the figure? TRUE
  or FALSE.

- show_plot:

  Print to the plot(s).

- bind_plots:

  Bind the list of plots together using
  [wrap_plots](https://patchwork.data-imaginist.com/reference/wrap_plots.html).

- wrap_width:

  Width at which to wrap x-axis text labels.

- verbose:

  Print messages.

## Value

List of two ggplot objects.

## Examples

``` r
res <- MAGMA.Celltyping::enrichment_results[[1]]
results <- res$ctAssocMerged$level1$results
ctd <- ewceData::ctd()
#> see ?ewceData and browseVignettes('ewceData') for documentation
#> loading from cache
tile_res <- magma_tileplot(ctd=ctd, results=results)
#> Saving plot ==> /tmp/RtmpEkCLL9/MAGMA_Figures/Tileplots/CombinedRes_TilePlot_MAGMA_wtDendro_level1_.pdf
```
