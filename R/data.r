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
#' This method uses \link[orthogene]{all_genes} and contains >1,200 more genes
#' than \code{hgnc2entrez} due to querying a more comprehensive
#'  and frequently updated database.
#'
#' @source
#' The code to prepare the .Rda file file from the marker file is:
#' \code{
#' #### OLD METHOD: biomaRt ####
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
#' 
#' #### NEW METHOD: orthogene ####
#' library(MAGMA.Celltyping); library(orthogene); library(dplyr);
#' gene_map <- orthogene::all_genes(species = "human",
#'                                  method = "gprofiler",
#'                                  target = "ENTREZGENE_ACC",
#'                                  ensure_filter_nas = FALSE)
#' hgnc2entrez_orthogene <- gene_map %>%
#'     dplyr::select(hgnc_symbol = Gene.Symbol,
#'                   entrez = target) %>%
#'     unique()
#' #### Compare to other dataset ####
#' dt1 <- hgnc2entrez %>% dplyr::filter(!hgnc_symbol %in% c(NA,""),
#'                                      !entrez %in% c(NA,"")) %>%
#'     unique()
#' dt2 <- hgnc2entrez_orthogene %>% dplyr::filter(!hgnc_symbol %in% c(NA,""),
#'                                                !entrez %in% c(NA,"")) %>%
#'     unique()
#' message("hgnc2entrez_orthogene has ",
#'         formatC(nrow(dt2) - nrow(dt1), big.mark = ","),
#'         " more genes than original method.")
#' usethis::use_data(hgnc2entrez_orthogene, overwrite = TRUE)
#' }
#' @usage data("hgnc2entrez_orthogene")
"hgnc2entrez_orthogene"
 

#### DEPRECATED  (replaced by hgnc2entrez_orthogene) ####
# All HGNC gene symbols with ENTREZ gene IDs
#
# A dataset containing all HGNC symbols in first column,
# then entrez in second column.
#
# @source
# The code to prepare the .Rda file file from the marker file is:
# \code{
# #library("biomaRt")
# #listMarts(host="www.ensembl.org")
# #human <- useMart(host="www.ensembl.org",
# #"ENSEMBL_MART_ENSEMBL", dataset="hsapiens_gene_ensembl")
# #all_hgnc_wtEntrez = getBM(attributes=c("hgnc_symbol","entrezgene"),
# #mart=human)
# download.file("ftp://ftp.ncbi.nlm.nih.gov/gene/DATA/gene_info.gz")
# # Note, 9606 is the identifier for human, use 10090 for mouse
# system(paste("gzip -cd gene_info.gz | awk \'BEGIN {FS=\"\t\"} $1==9606",
#              "{print $2 \"\t\" $3 \"\t\" $10}\' > geneInfo.txt)"))
#
# allHGNC=read.table(file="/Users/natske/geneInfo.txt",fill  = TRUE)
# all_hgnc_wtEntrez = allHGNC[,seq(2,1)]
# colnames(all_hgnc_wtEntrez) = c("hgnc_symbol","entrezgene")
# all_hgnc_wtEntrez = unique(all_hgnc_wtEntrez)
# usethis::use_data(all_hgnc_wtEntrez,overwrite = TRUE)
# }
# @usage data("all_hgnc_wtEntrez")
# "all_hgnc_wtEntrez"


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
#'  \link[MungeSumstats]{format_sumstats}.
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


#' Example celltype enrichment results
#' 
#' Example results from \link[MAGMA.Celltyping]{celltype_associations_pipeline}.
#' 
#' @source 
#' \code{ 
#' #### Import data ####
#' magma_dirs <- MAGMA.Celltyping::import_magma_files(ids = "ieu-a-298")
#' ctd <- ewceData::ctd()
#' enrichment_results <- MAGMA.Celltyping::celltype_associations_pipeline(
#'     magma_dirs = magma_dirs,
#'     ctd = ctd, 
#'     ctd_species = "mouse", 
#'     ctd_name = "Zeisel2015", 
#'     run_linear = TRUE, 
#'     run_top10 = TRUE)
#' usethis::use_data(enrichment_results, overwrite = TRUE)
#' }
#' @usage data("enrichment_results")
"enrichment_results"


#' Stored links to MAGMA executables
#' 
#' Stored links to MAGMA executables in the 
#' \href{https://ctg.cncr.nl/software/MAGMA/prog/archive/}{MAGMA archives}.
#' 
#' @source \href{https://ctg.cncr.nl/software/magma}{MAGMA}
#' @source 
#' \code{
#' magma_links_stored <- magma_links_gather()
#' usethis::use_data(magma_links_stored, overwrite = TRUE)
#' }
#' @usage data("magma_links_stored")
"magma_links_stored"


#' MAGMA_Files_Public: metadata
#' 
#' Metadata for all pre-computed metadata files stored in the 
#' \href{https://github.com/neurogenomics/MAGMA_Files_Public}{
#' MAGMA_Files_Public} GitHub repository.
#' 
#' @source 
#' \code{
#' #### Check what files are available ####
#' magma_files_metadata <- data.table::fread(
#'     file.path("https://github.com/neurogenomics/MAGMA_Files_Public",
#'               "raw/master/metadata.csv"), drop = "V1"
#' )
#' usethis::use_data(magma_files_metadata, overwrite = TRUE)
#' }
#' @usage data("magma_files_metadata")
"magma_files_metadata"

