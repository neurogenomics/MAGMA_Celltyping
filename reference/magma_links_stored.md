# Stored links to MAGMA executables

Stored links to MAGMA executables in the [MAGMA
archives](https://ctg.cncr.nl/software/MAGMA/prog/archive/).

## Usage

``` r
data("magma_links_stored")
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 121
rows and 5 columns.

## Source

[MAGMA](https://ctg.cncr.nl/software/magma)

` magma_links_stored <- MAGMA.Celltyping:::magma_links_gather() usethis::use_data(magma_links_stored, overwrite = TRUE) `
