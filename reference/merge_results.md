# Gather enrichment results

Gather enrichment results from
[celltype_associations_pipeline](https://neurogenomics.github.io/MAGMA_Celltyping/reference/celltype_associations_pipeline.md)
and converted them into harmonised, machine-readable tables.

## Usage

``` r
merge_results(
  MAGMA_results,
  level = 1,
  dataset_name = NULL,
  species = "mouse",
  filetype = "ctAssocMerged",
  q_thresh = NULL,
  method = "fdr",
  save_dir = tempdir(),
  verbose = TRUE
)
```

## Arguments

- MAGMA_results:

  The output of
  [celltype_associations_pipeline](https://neurogenomics.github.io/MAGMA_Celltyping/reference/celltype_associations_pipeline.md).

- level:

  Which level in the CellTypeDataset (CTD) to show results for.

- dataset_name:

  \[Optional\] Name of the CellTypeDataset (CTD).

- species:

  Species that the CellTypeDataset (CTD) came from.

- filetype:

  Type of analysis to gather the results of. These correspond to the
  modes of data analysis in
  [celltype_associations_pipeline](https://neurogenomics.github.io/MAGMA_Celltyping/reference/celltype_associations_pipeline.md):

  - `"ctAssocsLinear"` : Linear mode results.

  - `"ctAssocsTop"` : Top 10% mode results.

  - `"ctAssocMerged"` : Merged linear and top 10% results.

  - `"ctCondAssocs"` : Conditional results.

  *Note:* Only those analyses that were actually run in
  [celltype_associations_pipeline](https://neurogenomics.github.io/MAGMA_Celltyping/reference/celltype_associations_pipeline.md)
  can be retrieved.

- q_thresh:

  Multiple-testing corrected p-value (q-value) filtering threshold.

- method:

  correction method, a
  [`character`](https://rdrr.io/r/base/character.html) string. Can be
  abbreviated.

- save_dir:

  Directory to save the gathered results to.

- verbose:

  Print messages.

## Examples

``` r
if (FALSE) { # \dontrun{
#### Use precomputed results: see ?enrichment_results for details  ####
MAGMA_results <- MAGMA.Celltyping::enrichment_results

#### Merge results ####
merged_res <- MAGMA.Celltyping::merge_results(MAGMA_results)
} # }
```
