# Test to see which cell types are more enriched in magmaPath2 than magmaPath1

Test to see which cell types are more enriched in magmaPath2 than
magmaPath1

## Usage

``` r
compare_trait_enrichments(
  magmaPath1 = NA,
  magmaPath2 = NA,
  magma1 = NA,
  magma2 = NA,
  annotLevel,
  ctd
)
```

## Arguments

- magmaPath1:

  A magma results file

- magmaPath2:

  A magma results file

- magma1:

  A magma results data structure

- magma2:

  A magma results data structure

- annotLevel:

  Annotation level of the CTD / results file to use

- ctd:

  Cell type data structure containing \$specificity_quantiles

## Value

File path for the genes.out file
