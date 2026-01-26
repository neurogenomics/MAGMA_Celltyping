# Merge MAGMA results

Used for instance when you want to merge results from 'Linear' and 'Top
10%' enrichment modes.

## Usage

``` r
merge_magma_results(ctAssoc1, ctAssoc2)
```

## Arguments

- ctAssoc1:

  Output from either
  [calculate_celltype_associations](https://neurogenomics.github.io/MAGMA_Celltyping/reference/calculate_celltype_associations.md)
  or
  [calculate_conditional_celltype_associations](https://neurogenomics.github.io/MAGMA_Celltyping/reference/calculate_conditional_celltype_associations.md).

- ctAssoc2:

  Output from either
  [calculate_celltype_associations](https://neurogenomics.github.io/MAGMA_Celltyping/reference/calculate_celltype_associations.md)
  or
  [calculate_conditional_celltype_associations](https://neurogenomics.github.io/MAGMA_Celltyping/reference/calculate_conditional_celltype_associations.md).

## Value

List with same format as output from either
[calculate_celltype_associations](https://neurogenomics.github.io/MAGMA_Celltyping/reference/calculate_celltype_associations.md)
or
[calculate_conditional_celltype_associations](https://neurogenomics.github.io/MAGMA_Celltyping/reference/calculate_conditional_celltype_associations.md).

## Examples

``` r
res <- MAGMA.Celltyping::enrichment_results
ctAssoc1 <- res[[1]]$ctAssocsLinear
ctAssoc2 <- res[[1]]$ctAssocsTop
ctAssocs <- merge_magma_results(ctAssoc1=ctAssoc1, ctAssoc2=ctAssoc2)
```
