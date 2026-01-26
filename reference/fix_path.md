# Fix path

Ensure a file/directory path can be understood by all OS types by
expanding "~" to an absolute path, and standardized slash directions
with [normalizePath](https://rdrr.io/r/base/normalizePath.html). This is
especially important for Windows.

## Usage

``` r
fix_path(x)
```

## Arguments

- x:

  File/directory path.
