#' Check input to MAGMA cell type analyses
#'
#' This code was functonalised because it is called by both baseline and conditional analysis.
#'
#' @param ctd Cell type data strucutre containing $quantiles
#' @param gwas_sumstats_path Filepath of the summary statistics file
#' @param analysis_name Used in filenames which area created
#' @param upstream_kb How many kb upstream of the gene should SNPs be included?
#' @param downstream_kb How many kb downstream of the gene should SNPs be included?
#' @param genome_ref_path Path to the folder containing the 1000 genomes .bed files (which can be downloaded from https://ctg.cncr.nl/software/MAGMA/ref_data/g1000_eur.zip)
#'
#' @return Nothing returned but sends error if there is a fault with the arguments
#'
#' @examples
#' check_inputs_to_magma_celltype_analysis(ctd,gwas_sumstats_path,analysis_name,upstream_kb,downstream_kb,genome_ref_path)
#'
#' @export
check_inputs_to_magma_celltype_analysis <- function(ctd,gwas_sumstats_path,analysis_name,upstream_kb,downstream_kb,genome_ref_path){
    sumstatsPrefix = sprintf("%s.%sUP.%sDOWN",gwas_sumstats_path,upstream_kb,downstream_kb)
    magmaPaths = get.magma.paths(gwas_sumstats_path,upstream_kb,downstream_kb)
    
    #####################
    # ERROR CHECKS:
    # - Does ctd have quantiles?
    for(annotLevel in 1:length(ctd)){
        if(!"quantiles" %in% names(ctd[[annotLevel]])){stop("CTD should have quantiles. Send to 'prepare.quantile.groups' before calling this function.")}
    }
    # - Is annotLevel within length(ctd)
    if(!annotLevel %in% 1:length(ctd)){stop("annotLevel does not correspond to a level available with ctd[[annotLevel]]")}
    # - Does genome_ref_path.bed exist?
    if(!file.exists(sprintf("%s.bed",genome_ref_path))){stop(sprintf("%s.bed does not exist",genome_ref_path))}
    # - Check the genes.raw file exists
    if(!file.exists(sprintf("%s.genes.out",magmaPaths$filePathPrefix))){stop(sprintf("%s.genes.out does not exist. Run map.snps.to.genes() before this function",magmaPaths$filePathPrefix))}
    #####################    
}