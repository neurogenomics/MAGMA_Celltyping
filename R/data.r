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

#' Celltype data (allKI)
#'
#' CTD file with cortex, hippocampus, hypothalamus and midbrain
#'
#' @source
#' # http://www.hjerling-leffler-lab.org/data/scz_singlecell/
#' usethis::use_data(ctd_allKI,overwrite=TRUE)
"ctd_allKI"

#' Celltype data (AIBS)
#'
#' CTD file with AIBS human cortex data
#'
#' @source
#' # http://www.hjerling-leffler-lab.org/data/scz_singlecell/
#' usethis::use_data(ctd_AIBS,overwrite=TRUE)
"ctd_AIBS"

#' Celltype data (DRONC_human)
#'
#' CTD file with DRONC-seq data from human
#'
#' @source
#' # http://www.hjerling-leffler-lab.org/data/scz_singlecell/
#' usethis::use_data(ctd_DRONC_human,overwrite=TRUE)
"ctd_DRONC_human"

#' Celltype data (DivSeq)
#'
#' CTD file with DivSeq data
#'
#' @source
#' usethis::use_data(ctd_DivSeq,overwrite = TRUE)
"ctd_DivSeq"

#' Celltype data (Tasic)
#'
#' CTD file with Tasic data
#'
#' @source
#' usethis::use_data(ctd_Tasic,overwrite = TRUE)
"ctd_Tasic"

#' Celltype data (DRONC_mouse)
#'
#' CTD file with DRONC-seq data from mice
#'
#' @source
#' # http://www.hjerling-leffler-lab.org/data/scz_singlecell/
#' usethis::use_data(ctd_DRONC_mouse,overwrite=TRUE)
"ctd_DRONC_mouse"

#' Celltype data (Blue Lake 2018 Frontal Cortex)
#'
#' CTD file with data from humans
#'
#' @source
#' # Generated on Nathan's Mac: ~/Single Cell Datasets/BlueLake2018
#' usethis::use_data(ctd_BlueLake2018_FrontalCortexOnly,overwrite=TRUE)
"ctd_BlueLake2018_FrontalCortexOnly"

#' Celltype data (Blue Lake 2018 Visual Cortex)
#'
#' CTD file with data from humans
#'
#' @source
#' # Generated on Nathan's Mac: ~/Single Cell Datasets/BlueLake2018
#' usethis::use_data(ctd_BlueLake2018_FrontalCortexOnly,overwrite=TRUE)
"ctd_BlueLake2018_VisualCortexOnly"

#' Celltype data (Saunders)
#'
#' CTD file with data from humans
#'
#' @source
#' # Generated on Nathan's Mac: ctd_SaundersWithLvl1.Rda
#' usethis::use_data(ctd_Saunders,overwrite=TRUE)
"ctd_Saunders"

#' Celltype data (Zeisel2018)
#'
#' CTD file with data from mice
#'
#' @source
#' # Generated on Nathan's Mac: ctd_ZieselAllLevels.Rda
#' usethis::use_data(ctd_Zeisel2018,overwrite=TRUE)
"ctd_Zeisel2018"





#' Rbfox binding genes in MGI format
#'
#' List of significant hits for Rbfox 1 2 or 3 binding from supplementary table 1 of HITS-CLIP and Integrative Modeling Define the Rbfox Splicing-Regulatory Network Linked to Brain Development and Autism. All with rbfox2 count greater than 4 or summed rbfox 1 and 3 greater than 12
#'
#' @source
#' rbfox_binding = read.csv("/Users/natske/Google Drive/DiseaseLists/Rbfox_binding.txt",stringsAsFactors = FALSE)[-1,1]
#' devtools::use_data(rbfox_binding,overwrite = TRUE)
"rbfox_binding"


