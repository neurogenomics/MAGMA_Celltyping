# Example GWAS: raw (pre-munged)

Write example GWAS summary statistics to disk.

## Usage

``` r
get_example_gwas_raw(
  storage_dir = tools::R_user_dir(package = "MAGMA.Celltyping", which = "cache"),
  trait = c("prospective_memory", "fluid_intelligence"),
  timeout = 60 * 5,
  verbose = TRUE
)
```

## Arguments

- timeout:

  How many seconds to wait before timeout.

- verbose:

  Print messages.

## Value

Path to sumstats
