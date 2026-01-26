# List files

List files on a remote server or website

## Usage

``` r
list_remote_files(URL, pattern = "*.zip")
```

## Source

[stackoverflow](https://stackoverflow.com/questions/15954463/read-list-of-file-names-from-web-into-r)

## Arguments

- URL:

  Web URL.

- pattern:

  Search pattern passed to
  [xpathSApply](https://rdrr.io/pkg/XML/man/getNodeSet.html).

## Value

List of remote files.
