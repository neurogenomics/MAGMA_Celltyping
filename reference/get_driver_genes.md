# Get genes driving significant MAGMA_celltyping results

Infers the genes driving significant cell-type-specific enrichment
results by computing the mean rank of the adjusted Z-score from the GWAS
gene annotation file (`"ADJ_ZSTAT"`) and the cell-type specificity score
from the CellTypeDataset (`"specificity_proportion"`).

## Usage

``` r
get_driver_genes(
  ctd,
  ctd_species = infer_ctd_species(ctd = ctd),
  prepare_ctd = TRUE,
  magma_res,
  GenesOut_dir,
  fdr_thresh = 0.05,
  n_genes = 100,
  spec_deciles = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- ctd:

  CellTypeData object

- ctd_species:

  Either 'human' or 'mouse'

- prepare_ctd:

  Whether to run
  [prepare_quantile_groups](https://neurogenomics.github.io/MAGMA_Celltyping/reference/prepare_quantile_groups.md)
  on the `ctd` first.

- magma_res:

  Merged results from
  [merge_results](https://neurogenomics.github.io/MAGMA_Celltyping/reference/merge_results.md).

- GenesOut_dir:

  Folder to search for *.genes.out* files implicated in `magma_res`.

- fdr_thresh:

  FDR threshold for `magma_res`.

- n_genes:

  Max number of drive genes to return per cell-type enrichment.

- spec_deciles:

  \[Optional\] Which *"specificity_proportion"* deciles to include when
  calculating driver genes. (10 = most specific).

- verbose:

  Print messages.

- ...:

  Arguments passed on to
  [`EWCE::standardise_ctd`](https://rdrr.io/pkg/EWCE/man/standardise_ctd.html)

  `dataset`

  :   CellTypeData. name.

  `input_species`

  :   Which species the gene names in `exp` come from. See
      [list_species](https://rdrr.io/pkg/EWCE/man/list_species.html) for
      all available species.

  `output_species`

  :   Which species' genes names to convert `exp` to. See
      [list_species](https://rdrr.io/pkg/EWCE/man/list_species.html) for
      all available species.

  `sctSpecies_origin`

  :   Species that the `sct_data` originally came from, regardless of
      its current gene format (e.g. it was previously converted from
      mouse to human gene orthologs). This is used for computing an
      appropriate backgrund.

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

  `method`

  :   R package to use for gene mapping:

      `"gprofiler"`

      :   Slower but more species and genes.

      `"homologene"`

      :   Faster but fewer species and genes.

      `"babelgene"`

      :   Faster but fewer species and genes. Also gives consensus
          scores for each gene mapping based on a several different data
          sources.

  `force_new_quantiles`

  :   By default, quantile computation is skipped if they have already
      been computed. Set `=TRUE` to override this and generate new
      quantiles.

  `force_standardise`

  :   If `ctd` has already been standardised, whether to rerun
      standardisation anyway (Default: `FALSE`).

  `remove_unlabeled_clusters`

  :   Remove any samples that have numeric column names.

  `numberOfBins`

  :   Number of non-zero quantile bins.

  `keep_annot`

  :   Keep the column annotation data if provided.

  `keep_plots`

  :   Keep the dendrograms if provided.

  `as_sparse`

  :   Convert to sparse matrix.

  `as_DelayedArray`

  :   Convert to `DelayedArray`.

  `rename_columns`

  :   Remove `replace_chars` from column names.

  `make_columns_unique`

  :   Rename each columns with the prefix `dataset.species.celltype`.

## Examples

``` r
ctd <- ewceData::ctd()
#> see ?ewceData and browseVignettes('ewceData') for documentation
#> loading from cache
GenesOut_dir <- MAGMA.Celltyping::import_magma_files()
#> Using built-in example files: ieu-a-298.tsv.gz.35UP.10DOWN
#> Returning MAGMA directories.
magma_res <- MAGMA.Celltyping::merge_results(
    MAGMA.Celltyping::enrichment_results)
#> Saving full merged results to ==> /tmp/RtmpEkCLL9/MAGMA_celltyping./.lvl1.csv
genesets <- MAGMA.Celltyping::get_driver_genes(ctd = ctd, 
                                               magma_res = magma_res, 
                                               GenesOut_dir = GenesOut_dir, 
                                               fdr_thresh = 1)
#> Filtering @ FDR< 1
#> 1 genes.out file(s) found.
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
#> + Finding driver genes for: ieu-a-298 GWAS x CTD
#> Importing genes.out file.
#> 4 genes without HGNC gene symbols were dropped.
#> 371 genes that are absent from the ctd were dropped.
#> Computing adjusted Z-statistic.
#> + Level 1
#> Preparing specificity matrix.
#> 7 cell-types selected.
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
#> Identifying top driver genes per cell-type association:
#>    astrocytes_ependymal : 100 driver genes
#>    endothelial_mural : 100 driver genes
#>    interneurons : 100 driver genes
#>    microglia : 100 driver genes
#>    oligodendrocytes : 100 driver genes
#>    pyramidal_CA1 : 100 driver genes
#>    pyramidal_SS : 100 driver genes
```
