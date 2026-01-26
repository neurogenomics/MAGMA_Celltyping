# Standardise the column headers in the Summary Statistics files

Use a reference data table of common column header names (stored as
`MAGMA.Celltyping::sumstatsColHeaders`) convert them to a standard set,
i.e. chromosome –\> CHR. This function does not check that all the
required column headers are present. The amended header is written
directly back into the file.

## Usage

``` r
standardise.sumstats.column.headers(path)
```

## Arguments

- path:

  File path for the summary statistics file.

## Value

The amended column headers (also the column headers will be written
directly into the summary statistics file)

## Examples

``` r
if (FALSE) { # \dontrun{ 
path <- MAGMA.Celltyping::get_example_gwas()
col_headers <- MAGMA.Celltyping::standardise.sumstats.column.headers(
    path = path)
} # }
```
