# Rbfox binding genes in MGI format

List of significant hits for Rbfox 1 2 or 3 binding from supplementary
table 1 of HITS-CLIP and Integrative Modeling Define the Rbfox
Splicing-Regulatory Network Linked to Brain Development and Autism. All
with rbfox2 count greater than 4 or summed rbfox 1 and 3 greater than
12.

## Usage

``` r
data("rbfox_binding")
```

## Format

An object of class `character` of length 4143.

## Source

` rbfox_binding = read.csv( "/Users/natske/Google Drive/DiseaseLists/Rbfox_binding.txt", stringsAsFactors = FALSE)[-1,1] usethis::use_data(rbfox_binding,overwrite = TRUE) `
