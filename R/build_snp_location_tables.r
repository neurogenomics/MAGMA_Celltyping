#' Build data table with RSID / CHR / BP / GenomeBuild for all Hapmap3 SNPs in GRCh37 and GRCh38
#'
#' @return Save SNP_LOC_DATA to package data
#'
#' @examples
#' build_snp_location_tables()
#' data(SNP_LOC_DATA)
#'
#' @import R.utils
#' @export
build_snp_location_tables.r <- function(){
    tmpF1 = tempfile()
    tmpF2 = tempfile()
    download.file("https://data.broadinstitute.org/alkesgroup/LDSCORE/w_hm3.snplist.bz2",tmpF1)
    library(R.utils)
    gunzip(tmpF1,tmpF2)
    whm3 = read.table(tmpF2,stringsAsFactors = FALSE)
    hm3_rsids = whm3[,1]
    
    # Get GRCh37 locations
    source("https://bioconductor.org/biocLite.R")
    library("SNPlocs.Hsapiens.dbSNP144.GRCh37")
    snps <- SNPlocs.Hsapiens.dbSNP144.GRCh37
    hm3_rsids = hm3_rsids[grep("^rs",hm3_rsids)]
    snp_locs = snpsById(snps, hm3_rsids,ifnotfound="drop")
    SNP = mcols(snp_locs)$RefSNP_id
    CHR = seqnames(snp_locs)
    BP = pos(snp_locs)
    SNP_DATA_GRCh37 = data.frame(SNP=SNP,CHR=CHR,BP=BP,Build="GRCh37")
    devtools::use_data(SNP_DATA_GRCh37,overwrite = TRUE)
    
    # Get GRCh38 locations
    source("https://bioconductor.org/biocLite.R")
    biocLite("SNPlocs.Hsapiens.dbSNP144.GRCh38")
    library(SNPlocs.Hsapiens.dbSNP144.GRCh38)
    snps <- SNPlocs.Hsapiens.dbSNP144.GRCh38
    snp_locs = snpsById(snps, hm3_rsids,ifnotfound="drop")
    SNP = mcols(snp_locs)$RefSNP_id
    CHR = seqnames(snp_locs)
    BP = pos(snp_locs)
    SNP_DATA_GRCh38 = data.frame(SNP=SNP,CHR=CHR,BP=BP,Build="GRCh38")
    devtools::use_data(SNP_DATA_GRCh38,overwrite = TRUE)
    
    # Save to package /data/
    SNP_LOC_DATA = rbind(SNP_DATA_GRCh37,SNP_DATA_GRCh38)
    devtools::use_data(SNP_LOC_DATA,overwrite = TRUE)
    
    # # Get GRCh36.1 locations
    # source("https://bioconductor.org/biocLite.R")
    # biocLite("SNPlocs.Hsapiens.dbSNP.20090506")
    # library(SNPlocs.Hsapiens.dbSNP.20090506)
    # snp_locs = rsid2loc(hm3_rsids, caching=TRUE)
    # 
    # #snps <- SNPlocs.Hsapiens.dbSNP.20090506
    # snp_locs = snpsById(snps, hm3_rsids,ifnotfound="drop")
    # SNP = mcols(snp_locs)$RefSNP_id
    # CHR = seqnames(snp_locs)
    # BP = pos(snp_locs)
    # SNP_DATA_GRCh38 = data.frame(SNP=SNP,CHR=CHR,BP=BP,Build="GRCh38")
}