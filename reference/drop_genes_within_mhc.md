# Drop genes within MHC

Drop genes within MHC

## Usage

``` r
drop_genes_within_mhc(geneLocFilePath, geneLocFilePathOut)
```

## Arguments

- geneLocFilePath:

  The gene loc file as downloaded from MAGMA website (e.g.
  NCBI37.3.gene.loc).

- geneLocFilePathOut:

  The file path to write the gene loc file with MHC removed.

## Value

Null (the output is written to the filepath provided as an argument).

## Examples

``` r
gene_loc <- MAGMA.Celltyping:::get_genomeLocFile(build = "GRCH37")
#> ℹ All local files already up-to-date!
MAGMA.Celltyping::drop_genes_within_mhc(geneLocFilePath = gene_loc,
                                        geneLocFilePathOut = gene_loc)
```
