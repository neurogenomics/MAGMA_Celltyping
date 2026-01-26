# Standardise the column headers in the Summary Statistics files (CROSSPLATFORM)

Use a reference data table of common column header names (stored in
sumstatsColHeaders.rda) convert them to a standard set, i.e. chromosome
–\> CHR

## Usage

``` r
standardise.sumstats.column.headers.crossplatform(first_line)
```

## Arguments

- first_line:

  String containing the first line of the sumstats file

## Value

The amended column headers (also the column headers will be written
directly into the summary statistics file)

## Details

This function does not check that all the required column headers are
present

The amended header is written directly back into the file

## Examples

``` r
if (FALSE) { # \dontrun{
path <- MAGMA.Celltyping::get_example_gwas()
first_line <- readLines(path)[1]
col_headers <- MAGMA.Celltyping:::standardise.sumstats.column.headers.crossplatform(
     first_line = first_line)
} # }
```
