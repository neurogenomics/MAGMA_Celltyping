# Import GWAS summary statistics

Write example GWAS summary statistics to disk. All examples originally
come from the [UK Biobank](https://www.ukbiobank.ac.uk/). To reduce file
size, SNPs have been filtered to Minor Allele Frequency (MAF) \> 5
recommend using full GWAS summary statistics (after applying
[format_sumstats](https://al-murphy.github.io/MungeSumstats/reference/format_sumstats.html)).

## Usage

``` r
get_example_gwas(
  trait = c("educational_attainment", "fluid_intelligence", "prospective_memory"),
  munged = TRUE,
  storage_dir = tools::R_user_dir("MAGMA.Celltyping", which = "cache"),
  timeout = 60 * 5,
  verbose = TRUE
)
```

## Source

` #### fluid_intelligence #### gwas_sumstats_path <- MAGMA.Celltyping::get_example_gwas( trait = "fluid_intelligence", munged = FALSE) path_formatted <- MungeSumstats::format_sumstats( path=gwas_sumstats_path, save_path = tempfile(fileext = ".formatted.tsv.gz"), ref_genome ="GRCh37") ss <- data.table::fread(path_formatted) ss2 <- ss[MINOR_AF>=.05 & P<.05,] data.table::fwrite(ss2, path_formatted, sep = "\t") piggyback::pb_upload(file = path_formatted, name = "fluid_intelligence.ukb.tsv.gz", repo = "neurogenomics/MAGMA_Celltyping", overwrite = TRUE)#### prospective_memory #### gwas_sumstats_path <- MAGMA.Celltyping::get_example_gwas( trait = "prospective_memory", munged = FALSE) path_formatted <- MungeSumstats::format_sumstats( path=gwas_sumstats_path, save_path = tempfile(fileext = ".formatted.tsv.gz"), ref_genome ="GRCh37") ss <- data.table::fread(path_formatted) ss2 <- ss[MINOR_AF>=.05 & P<.05,] data.table::fwrite(ss2, path_formatted, sep = "\t") piggyback::pb_upload(file = path_formatted, name = "prospective_memory.ukb.tsv.gz", repo = "neurogenomics/MAGMA_Celltyping", overwrite = TRUE) `

## Arguments

- trait:

  Which trait to get GWAS summary stats for.

- munged:

  Whether to download the raw or pre-munged version of each GWAS
  (*Default:* `TRUE`).

- storage_dir:

  Folder in which to store the GWAS summary stats.

- timeout:

  How many seconds to wait before timeout.

- verbose:

  Print messages.

## Value

Path to downloaded GWAS summary statistics.

## Examples

``` r
path_formatted <- MAGMA.Celltyping::get_example_gwas()
#> Importing munged GWAS summary statistics: educational_attainment
```
