build_snp_location_tables.r <- function(){
    tmpF1 = tempfile()
    tmpF2 = tempfile()
    download.file("https://data.broadinstitute.org/alkesgroup/LDSCORE/w_hm3.snplist.bz2",tmpF1)
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
    
    # Get GRCh38 locations
    library(SNPlocs.Hsapiens.dbSNP144.GRCh38)
    hm3_rsids = hm3_rsids[grep("^rs",hm3_rsids)]
    snp_locs = snpsById(snps, hm3_rsids,ifnotfound="drop")
    SNP = mcols(snp_locs)$RefSNP_id
    CHR = seqnames(snp_locs)
    BP = pos(snp_locs)
    SNP_DATA_GRCh37 = data.frame(SNP=SNP,CHR=CHR,BP=BP,Build="GRCh37")
    
}