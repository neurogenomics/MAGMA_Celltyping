# Run MAGMA

Find the local MAGMA executable for a specific OS and run a command with
it.

## Usage

``` r
magma_run(version = NULL, cmd = NULL, verbose = FALSE)
```

## Source

[MAGMA archive](https://ctg.cncr.nl/software/MAGMA/prog/archive/)

## Arguments

- version:

  MAGMA version to use.

- cmd:

  Commands to MAGMA as a string or list of strings.

- verbose:

  Print messages.

## Examples

``` r
MAGMA.Celltyping::magma_run(cmd = "--version")
```
