#' Summary Statistics Column Headers
#'
#' List of uncorrected column headers often found in GWAS
#' Summary Statistics column headers.
#'
#' @source \href{https://github.com/neurogenomics/MungeSumstats}{MungeSumstats}
#' @source
#' \code{
#' # Most the data in the below table comes from the LDSC github wiki
#' sumstatsColHeaders = read.csv("inst/extdata/Magma_Column_headers.csv",
#'                               stringsAsFactors = FALSE)
#' usethis::use_data(sumstatsColHeaders,overwrite = TRUE)
#' }
#' @usage data("sumstatsColHeaders")
"sumstatsColHeaders"


#' HGNC to Entrez symbol mapping
#'
#' @source
#' The code to prepare the .Rda file file from the marker file is:
#' \code{
#' library("biomaRt")
#' human <- useMart(host="www.ensembl.org",
#'                  "ENSEMBL_MART_ENSEMBL",
#'                  dataset="hsapiens_gene_ensembl")
#' attrib_hum = listAttributes(human)
#' hgnc_symbols = getBM(attributes=c("hgnc_symbol","entrezgene"), mart=human)
#' colnames(hgnc_symbols) = c("hgnc_symbol","entrez")
#' hgnc_symbols = hgnc_symbols[hgnc_symbols$hgnc_symbol!=""]
#' hgnc_symbols = hgnc_symbols[!is.na(hgnc_symbols$entrez),]
#' hgnc2entrez = hgnc_symbols
#' usethis::use_data(hgnc2entrez,overwrite = TRUE)
#' }
#' @usage data("hgnc2entrez")
"hgnc2entrez"


#' Example of genesOut file
#'
#' Obtained from \url{'/Users/natske/OneDrive - Imperial College London/GWAS_Summary_Statistics/MAGMA_Files/20016.assoc.tsv.10UP.1.5DOWN/20016.assoc.tsv.10UP.1.5DOWN.genes.out'}
#'
#' @source
#' \code{
#' genesOut = data.table::fread(
#'     "/home/nskene/tmp_files/20016.assoc.tsv.10UP.1.5DOWN.genes.out")
#' usethis::use_data(genesOut,overwrite = TRUE)
#' }
#' @usage data("genesOut")
"genesOut"


#' All HGNC gene symbols with ENTREZ gene IDs
#'
#' A dataset containing all HGNC symbols in first column,
#' then entrez in second column.
#'
#' @source
#' The code to prepare the .Rda file file from the marker file is:
#' \code{
#' #library("biomaRt")
#' #listMarts(host="www.ensembl.org")
#' #human <- useMart(host="www.ensembl.org",
#' #"ENSEMBL_MART_ENSEMBL", dataset="hsapiens_gene_ensembl")
#' #all_hgnc_wtEntrez = getBM(attributes=c("hgnc_symbol","entrezgene"),
#' #mart=human)
#' download.file("ftp://ftp.ncbi.nlm.nih.gov/gene/DATA/gene_info.gz")
#' # Note, 9606 is the identifier for human, use 10090 for mouse
#' system(paste("gzip -cd gene_info.gz | awk \'BEGIN {FS=\"\t\"} $1==9606",
#'              "{print $2 \"\t\" $3 \"\t\" $10}\' > geneInfo.txt)"))
#'
#' allHGNC=read.table(file="/Users/natske/geneInfo.txt",fill  = TRUE)
#' all_hgnc_wtEntrez = allHGNC[,seq(2,1)]
#' colnames(all_hgnc_wtEntrez) = c("hgnc_symbol","entrezgene")
#' all_hgnc_wtEntrez = unique(all_hgnc_wtEntrez)
#' usethis::use_data(all_hgnc_wtEntrez,overwrite = TRUE)
#' }
#' @usage data("all_hgnc_wtEntrez")
"all_hgnc_wtEntrez"


#' Rbfox binding genes in MGI format
#'
#' List of significant hits for Rbfox 1 2 or 3 binding from supplementary
#' table 1 of HITS-CLIP and Integrative Modeling Define the Rbfox
#' Splicing-Regulatory Network Linked to Brain Development and Autism.
#' All with rbfox2 count greater than 4 or summed rbfox 1 and 3 greater than 12.
#'
#' @source
#' \code{
#'     rbfox_binding = read.csv(
#'         "/Users/natske/Google Drive/DiseaseLists/Rbfox_binding.txt",
#'         stringsAsFactors = FALSE)[-1,1]
#'     usethis::use_data(rbfox_binding,overwrite = TRUE)
#' }
#' @usage data("rbfox_binding")
"rbfox_binding"


#' Munged GWAS
#'
#' Example GWAS summary statistics standardised with
#'  \link[MungeSumstats]{format_sumtats}.
#' Original GWAS performed on Educational Attainment.
#'
#' @source
#' \code{
#' gwas_ss <- system.file("extdata", "eduAttainOkbay.txt",
#'                        package = "MungeSumstats")
#' gwas_munged_path <- MungeSumstats::format_sumstats(gwas_ss)
#' gwas_munged <- data.table::fread(gwas_munged_path)
#' usethis::use_data(gwas_munged, overwrite = TRUE)
#'  }
#' @usage data("gwas_munged")
"gwas_munged"
