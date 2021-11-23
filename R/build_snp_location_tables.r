#' Build SNP location tables
#'
#' Build data table with RSID / CHR / BP / GenomeBuild
#' for all Hapmap3 SNPs in GRCh37 and GRCh38.
#'
#' @return Save SNP_LOC_DATA to package data
#' 
#' @keywords internal
build_snp_location_tables <- function() {
    .Deprecated("MungeSumstats::get_genome_builds")
    requireNamespace("SNPlocs.Hsapiens.dbSNP144.GRCh37")
    requireNamespace("SNPlocs.Hsapiens.dbSNP144.GRCh38")
    requireNamespace("GenomeInfoDb")
    requireNamespace("BiocGenerics")
    requireNamespace("BSgenome")
    requireNamespace("utils")
    requireNamespace("S4Vectors")
    requireNamespace("data.table")
    
    tmpF1 <- tempfile()
    tmpF2 <- tempfile()
    utils::download.file(
        "https://ctg.cncr.nl/software/MAGMA/ref_data/g1000_eur.zip",
        tmpF1)
    utils::unzip(tmpF1, exdir = dirname(tmpF1))
    snpsALL <- data.table::fread(sprintf("%s/g1000_eur.bim", dirname(tmpF1)))
    g1000_snps <- as.character(snpsALL$V2)

    # tmpF1 = tempfile()
    # tmpF2 = tempfile()
    # download.file(
    #     "https://data.broadinstitute.org/alkesgroup/LDSCORE/w_hm3.snplist.bz2",
    #     tmpF1)
     
    # gunzip(tmpF1,tmpF2)
    # whm3 = read.table(tmpF2,stringsAsFactors = FALSE)
    # hm3_rsids = whm3[,1]

    # Get GRCh37 locations
    # source("https://bioconductor.org/biocLite.R") 
    snps <- SNPlocs.Hsapiens.dbSNP144.GRCh37::SNPlocs.Hsapiens.dbSNP144.GRCh37
    # g1000_snps = g1000_snps[grep("^rs",g1000_snps)]
    snp_locs <- BSgenome::snpsById(snps, g1000_snps, ifnotfound = "drop")
    SNP <- S4Vectors::mcols(snp_locs)$RefSNP_id
    CHR <- as.character(GenomeInfoDb::seqnames(snp_locs)) # seqnames(snp_locs)
    BP <- BiocGenerics::pos(snp_locs)
    SNP_DATA_GRCh37 <- data.frame(
        SNP = SNP,
        CHR = CHR,
        BP = BP,
        Build = "GRCh37",
        stringsAsFactors = FALSE,
        check.rows = FALSE,
        check.names = FALSE
    )
    # SNP_DATA_GRCh37 = data.table(SNP=SNP,CHR=CHR,BP=BP,Build="GRCh37")
    # usethis::use_data(SNP_DATA_GRCh37,overwrite = TRUE)

    # Get GRCh38 locations
    # source("https://bioconductor.org/biocLite.R")
    # BiocManager::install("SNPlocs.Hsapiens.dbSNP144.GRCh38") 
    snps <- SNPlocs.Hsapiens.dbSNP144.GRCh38::SNPlocs.Hsapiens.dbSNP144.GRCh38
    snp_locs <- BSgenome::snpsById(snps, g1000_snps, ifnotfound = "drop")
    SNP <- S4Vectors::mcols(snp_locs)$RefSNP_id
    CHR <- as.character(GenomeInfoDb::seqnames(snp_locs))
    BP <- BiocGenerics::pos(snp_locs)
    SNP_DATA_GRCh38 <- data.frame(
        SNP = SNP,
        CHR = CHR,
        BP = BP,
        Build = "GRCh38",
        stringsAsFactors = FALSE,
        check.rows = FALSE,
        check.names = FALSE
    )
    # usethis::use_data(SNP_DATA_GRCh38,overwrite = TRUE)

    # Save to package /data/
    SNP_LOC_DATA <- rbind(SNP_DATA_GRCh37, SNP_DATA_GRCh38)
    # usethis::use_data(SNP_LOC_DATA, overwrite = TRUE)
    # data.table::fwrite(SNP_LOC_DATA,file="data/SNP_LOC_DATA.csv")

    # # Get GRCh36.1 locations
    # source("https://bioconductor.org/biocLite.R")
    # biocLite("SNPlocs.Hsapiens.dbSNP.20090506") 
    # snp_locs = rsid2loc(g1000_snps, caching=TRUE)
    #
    # #snps <- SNPlocs.Hsapiens.dbSNP.20090506
    # snp_locs = snpsById(snps, g1000_snps,ifnotfound="drop")
    # SNP = mcols(snp_locs)$RefSNP_id
    # CHR = seqnames(snp_locs)
    # BP = pos(snp_locs)
    # SNP_DATA_GRCh38 = data.frame(SNP=SNP,CHR=CHR,BP=BP,Build="GRCh38")
}
