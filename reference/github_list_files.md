# List files

List files in a GitHub repository.

## Usage

``` r
github_list_files(
  user = "neurogenomics",
  repo = "MAGMA_Files_Public",
  branch = c("master", "main"),
  query = NULL,
  return_download_api = TRUE,
  verbose = TRUE
)
```

## Arguments

- user:

  Repository user name.

- repo:

  Repository name.

- branch:

  Which branch to search.

- query:

  A substring search to filter files with.

- return_download_api:

  Return the URL to download the file via GitHub's API, as opposed to
  the URL to view the file in a web browser.

- verbose:

  Print messages.

## Value

List of URLs.
