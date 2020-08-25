#' Get genome build for sumstats
#'
#' Compare the SNP:CHR:BP data against a dataset of all Hapmap3 SNP:CHR:BP data for two genome builds (GRCh37 and GRCh38). Evaluate which genome build matches the SNP locations from the sum stats file.
#'
#' @param path Filepath of the summary statistics file
#'
#' @return Either 'GRCh37' or 'GRCh38'
#'
#' @examples
#' genome_build = get_genomebuild_for_sumstats(gwas_sumstats_path)
#'
#' @export
#' @importFrom utils read.table
get_genomebuild_for_sumstats <- function(path){
    # Get first 20 lines of sumstats
    topLines = utils::read.table(path,nrows=30000,header = TRUE,stringsAsFactors = FALSE)
    topSNPS  = topLines$SNP
    topLOCs  = sprintf("%s-%s-%s",topLines$SNP,topLines$CHR,topLines$BP)
    SNP_LOC_DATA = load_snp_loc_data()
    sub_SNP_LOC_DATA = SNP_LOC_DATA[sample(1:dim(SNP_LOC_DATA)[1],100000),]
    topSNP_locs = sub_SNP_LOC_DATA[sub_SNP_LOC_DATA$SNP %in% topLines$SNP,]
    topSNP_locs$locs = sprintf("%s-%s-%s",topSNP_locs$SNP,topSNP_locs$CHR,topSNP_locs$BP)
    return(names(sort(table(topSNP_locs[topSNP_locs$locs %in% topLOCs,]$Build),decreasing = TRUE)[1]))
}