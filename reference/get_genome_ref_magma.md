# Download genome ref: magma

Download and decompress the genome reference needed to run
[map_snps_to_genes](https://neurogenomics.github.io/MAGMA_Celltyping/reference/map_snps_to_genes.md),
If the file already exists, it will simply return the path.

## Usage

``` r
get_genome_ref_magma(
  genome_ref_dir,
  population = "eur",
  timeout = 60 * 5,
  verbose = TRUE
)
```

## Source

[MAGMA site](https://cncr.nl/research/magma/)

## Arguments

- genome_ref_dir:

  Folder to download reference to.

- verbose:

  Print messages.
