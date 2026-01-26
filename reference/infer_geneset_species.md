# Infer gene set species

Infers species from from a gene set using
[infer_species](https://rdrr.io/pkg/orthogene/man/infer_species.html).
If `geneset_species` is not `NULL`, this will be returned instead of
inferring the species.

## Usage

``` r
infer_geneset_species(geneset, geneset_species = NULL, verbose = 1, ...)
```

## Arguments

- geneset:

  Genes which are to be tested. Can be gene symbols (or other gene IDs)
  from any species listed in
  [list_species](https://rdrr.io/pkg/EWCE/man/list_species.html).

- geneset_species:

  Species that the `geneset` came from. Can be any species listed in
  [list_species](https://rdrr.io/pkg/EWCE/man/list_species.html). If
  `geneset_species!="human"`, the `geneset` will be converted to 1:1
  human orthologs using
  [convert_orthologs](https://rdrr.io/pkg/orthogene/man/convert_orthologs.html).

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
  
geneset <- MAGMA.Celltyping::rbfox_binding
geneset_species <- infer_geneset_species(geneset = geneset)
#> geneset_species=NULL: Inferring species from gene names.
#> Using cached file: /github/home/.cache/R/orthogene/all_genes-9606-homologene.csv.gz
#> Using cached file: /github/home/.cache/R/orthogene/all_genes-9544-homologene.csv.gz
#> Using cached file: /github/home/.cache/R/orthogene/all_genes-10116-homologene.csv.gz
#> Using cached file: /github/home/.cache/R/orthogene/all_genes-10090-homologene.csv.gz
#> Using cached file: /github/home/.cache/R/orthogene/all_genes-7955-homologene.csv.gz
#> Using cached file: /github/home/.cache/R/orthogene/all_genes-7227-homologene.csv.gz
#> Inferred gene set species: mouse
```
