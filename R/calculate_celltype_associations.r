#' Calculate celltype associations using MAGMA
#'
#' Assumes that you have already run map.snps.to.genes()
#'
#' @param ctd Cell type data strucutre containing $quantiles
#' @param gwas_sumstats_path Filepath of the summary statistics file
#' @param analysis_name Used in filenames which area created
#' @param upstream_kb How many kb upstream of the gene should SNPs be included?
#' @param downstream_kb How many kb downstream of the gene should SNPs be included?
#' @param genome_ref_path Path to the folder containing the 1000 genomes .bed files (which can be downloaded from https://ctg.cncr.nl/software/MAGMA/ref_data/g1000_eur.zip)
#'
#' @return Filepath for the genes.out file
#'
#' @examples
#' ctAssocs = calculate_celltype_associations(ctd,gwas_sumstats_path)
#'
#' @export
calculate_celltype_associations <- function(ctd,gwas_sumstats_path,analysis_name="MainRun",upstream_kb=10,downstream_kb=1.5,genome_ref_path){
    sumstatsPrefix = sprintf("%s.%sUP.%sDOWN",gwas_sumstats_path,kb_upstream,kb_downstream)
    
    # Check for errors in arguments
    check_inputs_to_magma_celltype_analysis(ctd,gwas_sumstats_path,analysis_name,upstream_kb,downstream_kb,genome_ref_path)
    
    output = list()
    for(annotLevel in 1:length(ctd)){
        # First match quantiles to the genes in the genes.out file... then write as the genesCovar file (the input to MAGMA)
        geneCovarFile = create_gene_covar_file(genesOutFile = sprintf("%s.genes.out",sumstatsPrefix),ctd,annotLevel)
        
        sumstatsPrefix2 = sprintf("%s.level%s.%sUP.%sDOWN",gwas_sumstats_path,annotLevel,kb_upstream,kb_downstream)
        magma_cmd = sprintf("%smagma --gene-results '%s.genes.raw' --gene-covar '%s' onesided --out '%s.%s'",magma_path,sumstatsPrefix,geneCovarFile,sumstatsPrefix2,analysis_name)
        print(magma_cmd)
        system(magma_cmd)
        
        # Prepare output list
        tmp = list()
        tmp$geneCovarFile = geneCovarFile
        res = read.table(file=sprintf("%s.%s.gcov.out",sumstatsPrefix2,analysis_name),header=TRUE)
        res$P = as.numeric(res$P)
        res = res[order(res$P,decreasing = TRUE),]
        res$COVAR = factor(res$COVAR,levels=res$COVAR)
        res$CONTROL = "BASELINE"
        res$CONTROL_label = "BASELINE"
        res$ANNOTLEVEL=annotLevel
        tmp$results = res
        output[[length(output)+1]] = tmp
    }
    
    # Calculate total number of tests performed
    totalTests = 0
    for(annotLevel in 1:length(output)){
        totalTests = totalTests + dim(output[[annotLevel]]$results)[1]
    }
    output$total_baseline_tests_performed = totalTests
    
    output$gwas_sumstats_path = gwas_sumstats_path
    output$analysis_name = analysis_name
    output$upstream_kb = upstream_kb
    output$downstream_kb = downstream_kb
    output$genome_ref_path = genome_ref_path
    
    return(output)
}