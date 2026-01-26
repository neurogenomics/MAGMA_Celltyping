# Print messages

Conditionally print messages. Allows developers to easily control
verbosity of functions, and meet Bioconductor requirements that dictate
the message must first be stored to a variable before passing to
[message](https://rdrr.io/r/base/message.html).

## Usage

``` r
messager(..., v = TRUE, parallel = FALSE)
```

## Arguments

- v:

  Whether to print messages or not.

- parallel:

  Whether to enable message print when wrapped in parallelised
  functions.
