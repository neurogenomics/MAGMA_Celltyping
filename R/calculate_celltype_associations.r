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
#' @param specificity_species Species name relevant to the cell type data, i.e. "mouse" or "human"
#' @param genesOutCOND [Optional] Path to a genes.out file to condition on. Used if you want to condition on a different GWAS.
#' @param EnrichmentMode [Optional] Should either 'Linear' or 'Top 10%' mode be used for testing enrichment?
#'
#' @return Filepath for the genes.out file
#'
#' @examples
#' gwas_sumstats_path = "/Users/natske/Naomi_Wray_Conditional/Results/Conditional/scz_adj_all_withaut_gsmr_bxy_aric_reference.raw.NEW"
#' upstream_kb=10;downstream_kb=1.5;genome_ref_path="~/Downloads/g1000_eur/g1000_eur";specificity_species="mouse"
#' EnrichmentMode="Top 10%"
#' genesOutCOND = "/Users/natske/Naomi_Wray_Conditional/Results/Raw/scz_formtcojo.txt.NEW.10UP.1.5DOWN.genes.out"
#' ctAssocs = calculate_celltype_associations(ctd,gwas_sumstats_path)
#'
#' @export
calculate_celltype_associations <- function(ctd,gwas_sumstats_path,analysis_name="MainRun",upstream_kb=10,downstream_kb=1.5,genome_ref_path,specificity_species="mouse",genesOutCOND=NA,EnrichmentMode="Linear"){
    # Check EnrichmentMode has correct values
    if(!EnrichmentMode %in% c("Linear","Top 10%")){stop("EnrichmentMode argument must be set to either 'Linear' or 'Top 10%")}
    
    gwas_sumstats_path = path.expand(gwas_sumstats_path)
    magmaPaths = get.magma.paths(gwas_sumstats_path,upstream_kb,downstream_kb)
    
    # Check for errors in arguments
    check_inputs_to_magma_celltype_analysis(ctd,gwas_sumstats_path,analysis_name,upstream_kb,downstream_kb,genome_ref_path)
    
    output = list()
    for(annotLevel in 1:length(ctd)){
        sumstatsPrefix2 = sprintf("%s.level%s",magmaPaths$filePathPrefix,annotLevel)
        
        if(EnrichmentMode=="Linear"){
            # First match quantiles to the genes in the genes.out file... then write as the genesCovar file (the input to MAGMA)
            geneCovarFile = create_gene_covar_file(genesOutFile = sprintf("%s.genes.out",magmaPaths$filePathPrefix),ctd,annotLevel,specificity_species=specificity_species,genesOutCOND)
            
            if(is.na(genesOutCOND)){
                #magma_cmd = sprintf("magma --gene-results '%s.genes.raw' --gene-covar '%s' onesided --out '%s.%s'",magmaPaths$filePathPrefix,geneCovarFile,sumstatsPrefix2,analysis_name)
                magma_cmd = sprintf("magma --gene-results '%s.genes.raw' --gene-covar '%s' --out '%s.%s'",magmaPaths$filePathPrefix,geneCovarFile,sumstatsPrefix2,analysis_name)
            }else{
                magma_cmd = sprintf("magma --gene-results '%s.genes.raw' --gene-covar '%s' condition='ZSTAT' --out '%s.%s'",magmaPaths$filePathPrefix,geneCovarFile,sumstatsPrefix2,analysis_name)
            }
        }else if(EnrichmentMode=="Top 10%"){
            # First match quantiles to the genes in the genes.out file... then write as the genesCovar file (the input to MAGMA)
            geneCovarFile = create_top10percent_genesets_file(genesOutFile = sprintf("%s.genes.out",magmaPaths$filePathPrefix),ctd,annotLevel,specificity_species=specificity_species)
            
            if(is.na(genesOutCOND)){
                magma_cmd = sprintf("magma --gene-results '%s.genes.raw' --set-annot '%s' --out '%s.%s'",magmaPaths$filePathPrefix,geneCovarFile,sumstatsPrefix2,analysis_name)
            }else{
                geneCovarFile2 = create_gene_covar_file(genesOutFile = sprintf("%s.genes.out",magmaPaths$filePathPrefix),ctd,annotLevel,specificity_species=specificity_species,genesOutCOND)
                magma_cmd = sprintf("magma --gene-results '%s.genes.raw' --set-annot '%s' twosided --gene-covar '%s' condition-only='ZSTAT' --out '%s.%s'",magmaPaths$filePathPrefix,geneCovarFile,geneCovarFile2,sumstatsPrefix2,analysis_name)
            }
        }
        print(magma_cmd)
        system(magma_cmd)
        
        # Prepare output list
        tmp = list()
        tmp$geneCovarFile = geneCovarFile
        if(EnrichmentMode=="Linear"){
            path = sprintf("%s.%s.gcov.out",sumstatsPrefix2,analysis_name)
        }else if(EnrichmentMode=="Top 10%"){
            path = sprintf("%s.%s.sets.out",sumstatsPrefix2,analysis_name)
        }
        tmp$results = load.magma.results.file(path,annotLevel,ctd,genesOutCOND=genesOutCOND,EnrichmentMode=EnrichmentMode,ControlForCT="BASELINE")
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
