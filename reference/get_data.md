# Get remote data

Download remotely stored data via
[pb_download](https://docs.ropensci.org/piggyback/reference/pb_download.html).

## Usage

``` r
get_data(
  fname,
  repo = "neurogenomics/MAGMA_Celltyping",
  tag = "v2.0.8",
  .token = gh::gh_token(),
  storage_dir = tools::R_user_dir(package = "MAGMA.Celltyping", which = "cache"),
  overwrite = TRUE,
  check = FALSE
)
```

## Arguments

- repo:

  Repository name in format "owner/repo". Defaults to `guess_repo()`.

- tag:

  tag for the GitHub release to which this data should be attached.

- .token:

  GitHub authentication token, see `[gh::gh_token()]`

- overwrite:

  Should any local files of the same name be overwritten? default
  `TRUE`.
