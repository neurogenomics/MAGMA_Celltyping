# Get actual path

When given a file path, check that it exists and if not, search for
similarly named files and return those instead. This is important
because sometimes MAGMA adds a .txt. at the end of output files
(especially on Windows).

## Usage

``` r
get_actual_path(path)
```

## Arguments

- path:

  File path.
