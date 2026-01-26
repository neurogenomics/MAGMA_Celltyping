# Download files from GitHub

Download files stored in a public GitHub repository.

## Usage

``` r
github_download_files(
  filelist,
  save_dir = tempdir(),
  overwrite = FALSE,
  nThread = 1,
  verbose = TRUE
)
```

## Arguments

- filelist:

  List of URLs to files.

- save_dir:

  Local directory to save files in.

- overwrite:

  Overwrite existing local files.

- nThread:

  Number of threads to parallelise downloads over.

- verbose:

  Print messages.
