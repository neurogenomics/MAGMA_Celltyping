# Uninstall MAGMA

Uninstall one or all versions of MAGMA.

## Usage

``` r
magma_uninstall(version = NULL, verbose = TRUE)
```

## Arguments

- version:

  Which version of MAGMA to uninstall. If `NULL`, will uninstall all
  versions found.

- verbose:

  Print messages.

## Value

Path(s) to MAGMA installation folder(s) that were deleted.

## Examples

``` r
MAGMA.Celltyping::magma_uninstall()
#> Removing 1 installed versions of MAGMA.
#> [1] "/github/home/.cache/R/MAGMA.Celltyping/magma_v1.10"
```
