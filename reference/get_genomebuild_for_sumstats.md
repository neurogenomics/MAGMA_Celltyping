# Get genome build for sumstats

Compare the SNP:CHR:BP data against a dataset of all Hapmap3 SNP:CHR:BP
data for two genome builds (GRCh37 and GRCh38). Evaluate which genome
build matches the SNP locations from the sum stats file.

## Usage

``` r
get_genomebuild_for_sumstats(path)
```

## Arguments

- path:

  Filepath of the summary statistics file

## Value

Either 'GRCh37' or 'GRCh38'

## Examples

``` r
if (FALSE) { # \dontrun{
path <- MAGMA.Celltyping::get_example_gwas()
genome_build <- MAGMA.Celltyping::get_genomebuild_for_sumstats(path = path)
} # }
```
