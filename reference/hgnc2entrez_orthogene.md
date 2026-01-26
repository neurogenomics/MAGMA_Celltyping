# HGNC to Entrez symbol mapping

This method uses
[all_genes](https://rdrr.io/pkg/orthogene/man/all_genes.html) and
contains \>1,200 more genes than `hgnc2entrez` due to querying a more
comprehensive and frequently updated database.

## Usage

``` r
data("hgnc2entrez_orthogene")
```

## Format

An object of class `data.frame` with 25721 rows and 2 columns.

## Source

The code to prepare the .Rda file file from the marker file is:
` #### OLD METHOD: biomaRt #### library("biomaRt") human <- useMart(host="www.ensembl.org", "ENSEMBL_MART_ENSEMBL", dataset="hsapiens_gene_ensembl") attrib_hum = listAttributes(human) hgnc_symbols = getBM(attributes=c("hgnc_symbol","entrezgene"), mart=human) colnames(hgnc_symbols) = c("hgnc_symbol","entrez") hgnc_symbols = hgnc_symbols[hgnc_symbols$hgnc_symbol!=""] hgnc_symbols = hgnc_symbols[!is.na(hgnc_symbols$entrez),] hgnc2entrez = hgnc_symbols usethis::use_data(hgnc2entrez,overwrite = TRUE)#### NEW METHOD: orthogene #### library(MAGMA.Celltyping); library(orthogene); library(dplyr); gene_map <- orthogene::all_genes(species = "human", method = "gprofiler", target = "ENTREZGENE_ACC", ensure_filter_nas = FALSE) hgnc2entrez_orthogene <- gene_map |> dplyr::select(hgnc_symbol = Gene.Symbol, entrez = target) |> unique() #### Compare to other dataset #### dt1 <- hgnc2entrez |> dplyr::filter(!hgnc_symbol !entrez unique() dt2 <- hgnc2entrez_orthogene |> dplyr::filter(!hgnc_symbol !entrez unique() message("hgnc2entrez_orthogene has ", formatC(nrow(dt2) - nrow(dt1), big.mark = ","), " more genes than original method.") usethis::use_data(hgnc2entrez_orthogene, overwrite = TRUE) `
