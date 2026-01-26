# Import magma files as tables

Import magma *genes.out* and *genes.raw* output files as a nested list
of [data.table](https://rdrr.io/pkg/data.table/man/data.table.html)s.

## Usage

``` r
import_magma_files_tables(local_files, verbose = TRUE)
```

## Arguments

- local_files:

  A list of magma files, nested by GWAS ID.

- verbose:

  Print messages.
