# Infer CellTypeDataset species

Infers species from from level 1 of a CellTypeDataset (CTD) using either
the metadata stored in the CTD (if the object has previously been
standardised with
[standardise_ctd](https://rdrr.io/pkg/EWCE/man/standardise_ctd.html)) or
using the gene names (via
[infer_species](https://rdrr.io/pkg/orthogene/man/infer_species.html)).
If `ctd_species` is not `NULL`, this will be returned instead of
inferring the species.

## Usage

``` r
infer_ctd_species(ctd, ctd_species = NULL, verbose = 2, ...)
```

## Arguments

- ctd:

  Cell type data structure containing `specificity_quantiles`.

- ctd_species:

  Species name relevant to the CellTypeDataset (`ctd`). See
  [list_species](https://rdrr.io/pkg/EWCE/man/list_species.html) for all
  available species. If `ctd_species=NULL` (default), the `ctd` species
  will automatically be inferred using
  [infer_species](https://rdrr.io/pkg/orthogene/man/infer_species.html).

- verbose:

  Message verbosity.

  - `0` or `FALSE` : Don't print any messages.

  - `1` or `TRUE` : Only print messages from MAGMA.Celltyping.

  - `2` or `c(TRUE,TRUE)` : Print messages from MAGMA.Celltyping and the
    internal orthogene function.

- ...:

  Arguments passed on to
  [`orthogene::infer_species`](https://rdrr.io/pkg/orthogene/man/infer_species.html)

  `gene_df`

  :   Data object containing the genes (see `gene_input` for options on
      how the genes can be stored within the object).  
      Can be one of the following formats:  

      `matrix`

      :   A sparse or dense matrix.

      `data.frame`

      :   A `data.frame`, `data.table`. or `tibble`.

      `list`

      :   A `list` or character `vector`.

      Genes, transcripts, proteins, SNPs, or genomic ranges can be
      provided in any format (HGNC, Ensembl, RefSeq, UniProt, etc.) and
      will be automatically converted to gene symbols unless specified
      otherwise with the `...` arguments.  
      *Note*: If you set `method="homologene"`, you must either supply
      genes in gene symbol format (e.g. "Sox2") OR set
      `standardise_genes=TRUE`.

  `gene_input`

  :   Which aspect of `gene_df` to get gene names from:  

      `"rownames"`

      :   From row names of data.frame/matrix.

      `"colnames"`

      :   From column names of data.frame/matrix.

      `<column name>`

      :   From a column in `gene_df`, e.g. `"gene_names"`.

  `test_species`

  :   Which species to test for matches with. If set to `NULL`, will
      default to a list of humans and 5 common model organisms. If
      `test_species` is set to one of the following options, it will
      automatically pull all species from that respective package and
      test against each of them:

      "homologene"

      :   20+ species (default)

      "gprofiler"

      :   700+ species

      "babelgene"

      :   19 species

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

  `make_plot`

  :   Make a plot of the results.

  `show_plot`

  :   Print the plot of the results.

## Value

Inferred species name.

## Examples

``` r
  
ctd_species <- infer_ctd_species(ctd = ewceData::ctd())
#> see ?ewceData and browseVignettes('ewceData') for documentation
#> loading from cache
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
```
