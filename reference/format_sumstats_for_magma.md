# Format GWAS summary statistics

Check that sumstats has correct columns and that they are in the correct
order for MAGMA and LDSC.

## Usage

``` r
format_sumstats_for_magma(path)
```

## Arguments

- path:

  Filepath for the summary statistics file to be formatted.

## Value

col_headers The new column headers for the sumstats file

## Examples

``` r
if (FALSE) { # \dontrun{
path <- MAGMA.Celltyping::get_example_gwas()
new_path <- MAGMA.Celltyping:::format.sumstats.for.magma(path)
} # }
```
