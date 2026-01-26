# Munged GWAS

Example GWAS summary statistics standardised with
[format_sumstats](https://al-murphy.github.io/MungeSumstats/reference/format_sumstats.html).
Original GWAS performed on Educational Attainment.

## Usage

``` r
data("gwas_munged")
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 93
rows and 9 columns.

## Source

` gwas_ss <- system.file("extdata", "eduAttainOkbay.txt", package = "MungeSumstats") gwas_munged_path <- MungeSumstats::format_sumstats(gwas_ss) gwas_munged <- data.table::fread(gwas_munged_path) usethis::use_data(gwas_munged, overwrite = TRUE) `
