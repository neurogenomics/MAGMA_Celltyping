# Download genome ref

Download and decompress the genome reference needed to run
[map_snps_to_genes](https://neurogenomics.github.io/MAGMA_Celltyping/reference/map_snps_to_genes.md),
If the file already exists, it will simply return the path.

## Usage

``` r
get_genome_ref(
  genome_ref_path = NULL,
  storage_dir = tools::R_user_dir("MAGMA.Celltyping", which = "cache"),
  method = c("magma"),
  population = c("eur", "afr", "amr", "eas", "sas"),
  timeout = 60 * 15,
  verbose = TRUE
)
```

## Arguments

- genome_ref_path:

  If not `NULL` and file exists, this file path will be returned.

- storage_dir:

  Where to store genome ref.

- method:

  Get reference genome data from the [MAGMA
  server](https://ctg.cncr.nl/software/MAGMA/ref_data/) (slow).

- population:

  Which population subset of the genome reference to include.

  - "eur" : European descent (Default simply because this is currently
    the most common GWAS subpopulation).

  - "afr" : African descent.

  - "amr" : Ad Mixed American descent.

  - "eas" : East Asian descent.

  - "sas" : South Asian descent.

- timeout:

  Number seconds to wait before ending the download (Default: 5
  minutes).

- verbose:

  Print messages.

## Value

Directory where the genome reference data is stored.

## Examples

``` r
if (FALSE) { # \dontrun{
genome_ref_path <- get_genome_ref()
} # }
```
