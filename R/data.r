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
#' #library("biomaRt")
#' #listMarts(host="www.ensembl.org")
#' #human <- useMart(host="www.ensembl.org", "ENSEMBL_MART_ENSEMBL", dataset="hsapiens_gene_ensembl")
#' #all_hgnc_wtEntrez = getBM(attributes=c("hgnc_symbol","entrezgene"), mart=human)
#' wget ftp://ftp.ncbi.nlm.nih.gov/gene/DATA/gene_info.gz
#' gzip -cd gene_info.gz | awk 'BEGIN {FS="\t"} $1==9606 {print $2 "\t" $3 "\t" $10}' > geneInfo.txt # Note, 9606 is the identifier for human, use 10090 for mouse
#' allHGNC=read.table(file="/Users/natske/geneInfo.txt",fill  = TRUE)
#' all_hgnc_wtEntrez = allHGNC[,2:1]
#' colnames(all_hgnc_wtEntrez) = c("hgnc_symbol","entrezgene")
#' all_hgnc_wtEntrez = unique(all_hgnc_wtEntrez)
#' devtools::use_data(all_hgnc_wtEntrez,overwrite = TRUE)
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

