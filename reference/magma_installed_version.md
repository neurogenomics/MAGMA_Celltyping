# MAGMA installed version

List which version(s) of MAGMA is currently installed.

## Usage

``` r
magma_installed_version(
  magma_x = magma_executable(),
  as_package_version = FALSE,
  verbose = TRUE
)
```

## Arguments

- magma_x:

  MAGMA executable.

- as_package_version:

  Return as the object class
  [package_version](https://rdrr.io/r/base/numeric_version.html). NOTE,
  this will convert any character suffixes into numbers: e.g. `"v1.06a"`
  –\> `"v1.06.1"`, or `"v1.06b"` –\> `"v1.06.2"`

- verbose:

  Print messages.
