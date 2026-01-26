# Download genome ref: piggyback

Download and decompress the genome reference needed to run
[map_snps_to_genes](https://neurogenomics.github.io/MAGMA_Celltyping/reference/map_snps_to_genes.md),
If the file already exists, it will simply return the path.

## Usage

``` r
get_sub_SNP_LOC_DATA(
  storage_dir = tools::R_user_dir(package = "MAGMA.Celltyping", which = "cache")
)
```

## Source

` URL <- paste( "https://github.com/neurogenomics/MAGMA_Celltyping/raw/master/", "data/sub_SNP_LOC_DATA.rda", sep="/") tmp <- file.path(tempdir(),basename(URL)) piggyback::pb_upload(file = tmp, repo = "neurogenomics/MAGMA_Celltyping", overwrite = TRUE) `
