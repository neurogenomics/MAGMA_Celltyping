# Copy MAGMA gene mapping files to new folder

Copy SNPs-to-genes mapping files (*.genes.out*) produced by
[map_snps_to_genes](https://neurogenomics.github.io/MAGMA_Celltyping/reference/map_snps_to_genes.md)
to a new folder. Importantly, this function maintains the file naming
and folder structure conventions necessary to run other downstream
MAGMA.Celltyping functions.

## Usage

``` r
copy_snps_to_genes_files(
  gene_files,
  save_dir = "MAGMA_Files",
  overwrite = FALSE
)
```

## Arguments

- gene_files:

  Full paths to gene mapping mapping files.

- save_dir:

  Where to save the gene mapping files to.

- overwrite:

  Whether to overwrite gene mapping files that already exist in
  `save_dir` (Default: `FALSE`).

## Value

Named character list
