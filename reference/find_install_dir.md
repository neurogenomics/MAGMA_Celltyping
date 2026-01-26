# Find a directory to install software in

Find a directory that you have write permissions in to so that you can
install software there. Tests several options and returns the first
viable one.

## Usage

``` r
find_install_dir(
  dest_dir_opts = c(tools::R_user_dir("MAGMA.Celltyping", which = "cache"),
    system.file("tools", package = "MAGMA.Celltyping"), "/usr/local/bin",
    Sys.getenv("HOME"), getwd(), tempdir()),
  verbose = FALSE
)
```

## Arguments

- dest_dir_opts:

  Potential installation directory options.

- verbose:

  Print messages.

## Value

Path to viable installation directory.
