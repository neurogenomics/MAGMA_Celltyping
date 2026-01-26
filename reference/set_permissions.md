# Set permissions

Robust methods for setting file/folder permissions across multiple OS.

## Usage

``` r
set_permissions(path, is_folder = FALSE, verbose = TRUE)
```

## Arguments

- path:

  Path to file/folder.

- is_folder:

  Whether `path` is a folder. If so, permissions will be set recursively
  for all files/subfolders.

- verbose:

  Print messages.
