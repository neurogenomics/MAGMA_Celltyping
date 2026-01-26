# Fix celltype names

Make sure celltypes don't contain characters that could interfere with
downstream analyses. For example, the R package
[MAGMA.Celltyping](https://github.com/neurogenomics/MAGMA_Celltyping)
cannot have spaces in celltype names because spaces are used as a
delimiter in later steps.

## Usage

``` r
fix_celltype_names2(
  celltypes,
  replace_chars = "[-]|[.]|[ ]|[//]|[\\/]",
  make_unique = TRUE
)
```

## Arguments

- celltypes:

  Character vector of celltype names.

- replace_chars:

  Regex string of characters to replace with "\_" when renaming columns.

- make_unique:

  Make all entries unique.

## Value

Fixed celltype names.
