# Get gene info

Get gene info from Biomart

## Usage

``` r
get_gene_info(genelist, attributes = c("size"), one_row_per_gene = FALSE)
```

## Arguments

- genelist:

  Gene list.

- attributes:

  Attributes you want to retrieve. A possible list of attributes can be
  retrieved using the function
  [`listAttributes()`](https://huber-group-embl.github.io/biomaRt/reference/listAttributes.html).
