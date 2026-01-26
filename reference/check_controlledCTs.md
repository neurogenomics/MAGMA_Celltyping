# Check controlled cell type names

Check the cell types in 'controlledCT' exist at the relevant annotation
level.

## Usage

``` r
check_controlledCTs(ctd, controlledCTs, controlledAnnotLevel)
```

## Arguments

- ctd:

  Cell type data structure containing `specificity_quantiles`.

- controlledCTs:

  Array of the celltype to be controlled for, e.g. c("Interneuron type
  16","Medium Spiny Neuron).

- controlledAnnotLevel:

  Annotation level of the celltypes being controlled for.

## Value

Null output.
