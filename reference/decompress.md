# Decompress a file

Decompress a gzipped (.gz) or bgzipped (.bgz) file.

## Usage

``` r
decompress(
  path_formatted,
  remove = FALSE,
  overwrite = TRUE,
  storage_dir = tempdir(),
  verbose = TRUE
)
```

## Arguments

- path_formatted:

  Path to compressed file.

- remove:

  If [`TRUE`](https://rdrr.io/r/base/logical.html), the input file is
  removed afterward, otherwise not.

- overwrite:

  If [`TRUE`](https://rdrr.io/r/base/logical.html) and the output file
  already exists, the file is silently overwritten, otherwise an
  exception is thrown (unless `skip` is
  [`TRUE`](https://rdrr.io/r/base/logical.html)).

- storage_dir:

  Folder in which to store the GWAS summary stats.

- verbose:

  Print messages.

## Value

Path to decompressed file.
