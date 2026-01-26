# Download genome ref: piggyback

Download and decompress the genome reference needed to run
[map_snps_to_genes](https://neurogenomics.github.io/MAGMA_Celltyping/reference/map_snps_to_genes.md),
If the file already exists, it will simply return the path.

## Usage

``` r
get_genome_ref_piggyback(genome_ref_dir, verbose = TRUE, ...)
```

## Source

` genome_ref_path <- get_genome_ref(method = "magma")piggyback::pb_upload(file = genome_ref_path, repo = "neurogenomics/MAGMA_Celltyping", overwrite = TRUE) `

## Arguments

- genome_ref_dir:

  Folder to download reference to.

- verbose:

  Print messages.

- ...:

  Arguments passed on to
  [`get_data`](https://neurogenomics.github.io/MAGMA_Celltyping/reference/get_data.md)

  `repo`

  :   Repository name in format "owner/repo". Defaults to
      `guess_repo()`.

  `tag`

  :   tag for the GitHub release to which this data should be attached.

  `overwrite`

  :   Should any local files of the same name be overwritten? default
      `TRUE`.

  `.token`

  :   GitHub authentication token, see `[gh::gh_token()]`
