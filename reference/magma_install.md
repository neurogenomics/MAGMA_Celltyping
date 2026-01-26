# Install the MAGMA command line tool

Checks whether MAGMA is currently installed, and if not, tries to
automatically install it. For a list of all changes across MAGMA
versions, please see
[here](https://ctg.cncr.nl/software/MAGMA/doc/changelog.txt).

## Usage

``` r
magma_install(
  dest_dir = find_install_dir(),
  desired_version = "latest",
  upgrade = FALSE,
  verbose = TRUE
)
```

## Source

[MAGMA website](https://ctg.cncr.nl/software/magma)

[MAGMA.celltyping
documentation](https://github.com/neurogenomics/MAGMA_Celltyping)

## Arguments

- dest_dir:

  Folder in which to install MAGMA.

- desired_version:

  Desired version of MAGMA.

- upgrade:

  If MAGMA is already installed and is not the latest version, should it
  be upgraded to the latest version? (Default: `FALSE`).

- verbose:

  Print messages.

## Value

Path to MAGMA executable.

## Examples

``` r
magma <- magma_install()
#> Installed MAGMA version: v1.10
#> Skipping MAGMA installation.
#> The desired_version of MAGMA is currently installed: v1.10
#> Using: magma_v1.10
```
