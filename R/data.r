#' Summary Statistics Column Headers
#'
#' List of uncorrected column headers often found in GWAS Summary Statistics column headers
#'
#' @source
#' The code to prepare the .Rda file file from the marker file is:
#' \code{
#' # Most the data in the below table comes from the LDSC github wiki
#' sumstatsColHeaders = read.csv("Magma_Column_headers.csv",stringsAsFactors = FALSE)
#' devtools::use_data(sumstatsColHeaders,overwrite = TRUE)
#' }
#'
"sumstatsColHeaders"

#' All HGNC gene symbols with ENTREZ gene IDs
#'
#' A dataset containing all HGNC symbols in first column, then entrez in second column
#'
#' @source
#' The code to prepare the .Rda file file from the marker file is:
#' \code{
#' library("biomaRt")
#' listMarts(host="www.ensembl.org")
#' human <- useMart(host="www.ensembl.org", "ENSEMBL_MART_ENSEMBL", dataset="hsapiens_gene_ensembl")
#' all_hgnc_wtEntrez = getBM(attributes=c("hgnc_symbol","entrezgene"), mart=human)
#' devtools::use_data(all_hgnc_wtEntrez)
#' }
#'
"all_hgnc_wtEntrez"

#' RSD / CHR / BP for GRCh37 and GRCh38
#'
#' Data table with all Hapmap3 SNPs
#'
#' @source
#' build_snp_location_tables()
#'
"SNP_LOC_DATA"

