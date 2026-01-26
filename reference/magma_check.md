# MAGMA check

Check that MAGMA is installed at the beginning of functions where MAGMA
is required.

## Usage

``` r
magma_check(
  version = NULL,
  upgrade = FALSE,
  return_version = FALSE,
  verbose = TRUE
)
```

## Arguments

- version:

  MAGMA version.

- upgrade:

  If MAGMA is already installed and is not the latest version, should it
  be upgraded to the latest version? (Default: `FALSE`).

- return_version:

  Return the MAGMA version being used.

- verbose:

  Print messages.
