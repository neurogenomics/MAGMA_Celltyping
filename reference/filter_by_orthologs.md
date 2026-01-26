# Filter CellTypeDataset

Filter CellTypeDataset (CTD) specificity and mean_exp from CTD list to
only keep a subset of genes.

## Usage

``` r
filter_by_orthologs(ctd_oneLevel, one2one_ortholog_symbols)
```

## Source

` ctd <- ewceData::ctd() orths <- ewceData::mouse_to_human_homologs() ctd2 <- lapply(ctd, filter_by_orthologs, orths$MGI.symbol) `

## Arguments

- ctd_oneLevel:

  A single level of a CTD

- one2one_ortholog_symbols:

  character expansion for the text.

## Value

The CTD with both specificity and mean expression matrices filtered to
only contain 1:1 ortholog genes.

## Details

Created so that lapply can be used to do this.
