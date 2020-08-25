#' Calculate conditional celltype associations using MAGMA
#'
#' Assumes that you have already run map.snps.to.genes()
#'
#' @param ctd Cell type data strucutre containing $specificity_quantiles
#' @param gwas_sumstats_path Filepath of the summary statistics file
#' @param analysis_name Used in filenames which area created
#' @param upstream_kb How many kb upstream of the gene should SNPs be included?
#' @param downstream_kb How many kb downstream of the gene should SNPs be included?
#' @param genome_ref_path Path to the folder containing the 1000 genomes .bed files (which can be downloaded from https://ctg.cncr.nl/software/MAGMA/ref_data/g1000_eur.zip)
#' @param specificity_species Species name relevant to the cell type data, i.e. 'mouse' or 'human'
#' @param controlledAnnotLevel Which annotation level should be controlled for
#' @param controlTopNcells How many of the most significant cell types at that annotation level should be controlled for?
#' @param controlledCTs Array of the celltype to be controlled for, i.e. c('Interneuron type 16','Medium Spiny Neuron')
#' @param EnrichmentMode [Optional] Either 'Linear' or 'Top 10\%'. Default assumes Linear.
#'
#' @return Filepath for the genes.out file
#'
#' @examples
#' ctAssocs = calculate_celltype_associations(ctd,gwas_sumstats_path)
#'
#' @export
calculate_conditional_celltype_associations <- function(ctd,gwas_sumstats_path,analysis_name="MainRun",upstream_kb=10,downstream_kb=1.5,genome_ref_path,controlledAnnotLevel=1,specificity_species="mouse",controlTopNcells=NA,controlledCTs=NA,EnrichmentMode="Linear"){
    # Check EnrichmentMode has correct values
    if(!EnrichmentMode %in% c("Linear","Top 10%")){stop("EnrichmentMode argument must be set to either 'Linear' or 'Top 10%")}
    
    gwas_sumstats_path = path.expand(gwas_sumstats_path)
    magmaPaths = get.magma.paths(gwas_sumstats_path,upstream_kb,downstream_kb)
    
    # Check for errors in arguments
    check_inputs_to_magma_celltype_analysis(ctd,gwas_sumstats_path,analysis_name,upstream_kb,downstream_kb,genome_ref_path)
    
    # Either controlTopNcells or controlledCTs should be passed with arguments, not both
    suppressWarnings(if(!is.na(controlTopNcells) & !is.na(controlledCTs)){stop("Either controlTopNcells or controlledCTs should be passed with arguments, not both")})
    # If both are NA then also reject that
    suppressWarnings(if(is.na(controlTopNcells) & is.na(controlledCTs)){stop("Either controlTopNcells or controlledCTs should be passed with arguments")})
    
    # Calculate the baseline associations
    ctAssocs = calculate_celltype_associations(ctd,gwas_sumstats_path,genome_ref_path=genome_ref_path,specificity_species=specificity_species,EnrichmentMode = EnrichmentMode,upstream_kb=upstream_kb,downstream_kb=downstream_kb)
    
    if(!is.na(controlledCTs[1])){
        # Check if controlledCTs are all in the CTD at the expected annotation level
        if(mean(controlledCTs %in% colnames(ctd[[controlledAnnotLevel]]$specificity))<1){
            missingCTs = controlledCTs[!controlledCTs %in% colnames(ctd[[controlledAnnotLevel]]$specificity)]
            stop(sprintf("The following celltypes are not found at the specified annotation level: %s",paste(missingCTs,sep=" ")))
        }else{signifCells=controlledCTs}
    }else{
        # Find the cells which are most significant at baseline at controlled annotation level
        res = ctAssocs[[controlledAnnotLevel]]$results
        res = res[order(res$P),]
        signifCells = as.character(res[res$P<(0.05/ctAssocs$total_baseline_tests_performed),"Celltype"])
        
        if(length(signifCells)>controlTopNcells){
            signifCells = signifCells[1:controlTopNcells]
        }
        
        # If there are no significant cells... then stop
        if(length(signifCells)==0){stop("No celltypes reach significance with Q<0.05")}
    }
    
    # Create gene covar file for the controlled for annotation level
    controlledCovarFile = create_gene_covar_file(genesOutFile = sprintf("%s.genes.out",magmaPaths$filePathPrefix),ctd,controlledAnnotLevel,specificity_species=specificity_species)
    # Read in the controlled Covar File
    controlledCovarData = read.table(controlledCovarFile,stringsAsFactors = FALSE,header=TRUE)
    #colnames(controlledCovarData)[2:length(colnames(controlledCovarData))] = colnames(ctd[[controlledAnnotLevel]]$specificity_quantiles)
    transliterateMap = data.frame(original=colnames(ctd[[controlledAnnotLevel]]$specificity_quantiles),modified=colnames(controlledCovarData)[2:length(colnames(controlledCovarData))],stringsAsFactors = FALSE)
    if(!is.na(controlledCTs[1])){
        signifCells2 = transliterateMap[transliterateMap$original %in% signifCells,]$modified # Because full stops replace spaces when the covars are written to file... (and MAGMA sees spaces as delimiters)
    }else{
        signifCells2 = signifCells
    }
    controlledCovarCols = controlledCovarData[,c("entrez",signifCells2)]
    controlCovarFile=tempfile()
    write.table(controlledCovarCols,file=controlCovarFile,quote=FALSE,row.names=FALSE,sep="\t")
    
    for(annotLevel in 1:length(ctd)){
        count=allRes=0
        
        # First match quantiles to the genes in the genes.out file... then write as the genesCovar file (the input to MAGMA)
        if(EnrichmentMode=="Linear"){
            genesCovarFile = create_gene_covar_file(genesOutFile = sprintf("%s.genes.out",magmaPaths$filePathPrefix),ctd,annotLevel,specificity_species=specificity_species)
        }else{
            geneCovarFile = create_top10percent_genesets_file(genesOutFile = sprintf("%s.genes.out",magmaPaths$filePathPrefix),ctd,annotLevel,specificity_species=specificity_species)
        }
        
        # First control for each individually
        for(controlFor in signifCells2){
            if(EnrichmentMode=="Linear"){
                if(annotLevel!=controlledAnnotLevel){
                    genesCovarData = read.table(genesCovarFile,stringsAsFactors = FALSE,header=TRUE)
                    genesCovarData2 = merge(genesCovarData,controlledCovarCols[,c("entrez",controlFor)])
                    write.table(genesCovarData2,file=genesCovarFile,quote=FALSE,row.names=FALSE,sep="\t")
                }                
                
                sumstatsPrefix2 = sprintf("%s.level%s.%sUP.%sDOWN.Linear.ControlFor_%s",magmaPaths$filePathPrefix,annotLevel,upstream_kb,downstream_kb,controlFor)
                magma_cmd = sprintf("magma --gene-results '%s.genes.raw' --gene-covar '%s' --model direction=pos condition='%s' --out '%s'",magmaPaths$filePathPrefix,genesCovarFile,controlFor,sumstatsPrefix2)
            }else{
                controlledCovarCols2 = controlledCovarCols
                colnames(controlledCovarCols2)[-1] = sprintf("%s.covar",colnames(controlledCovarCols2)[-1])
                write.table(controlledCovarCols2,file=controlCovarFile,quote=FALSE,row.names=FALSE,sep="\t")
                controlledCTcovarNames = colnames(controlledCovarCols2)[-1]
                sumstatsPrefix2 = sprintf("%s.level%s.%sUP.%sDOWN.Top10.ControlFor_%s",magmaPaths$filePathPrefix,annotLevel,upstream_kb,downstream_kb,controlFor)
            
                # First match quantiles to the genes in the genes.out file... then write as the genesCovar file (the input to MAGMA)
                magma_cmd = sprintf("magma --gene-results '%s.genes.raw' --set-annot '%s' --gene-covar '%s' --model direction=pos  condition=%s --out '%s'",magmaPaths$filePathPrefix,geneCovarFile,controlCovarFile,sprintf("%s.covar",controlFor),sumstatsPrefix2)
            }
            
            print(magma_cmd)
            system(magma_cmd)    

            cond_res = load.magma.results.file(path=sprintf("%s.gsa.out",sumstatsPrefix2),annotLevel,ctd,genesOutCOND=NA,EnrichmentMode=EnrichmentMode,ControlForCT=controlFor)
            count = count + 1
            if(count==1){
                allRes = cond_res
            }else{
                allRes = rbind(allRes,cond_res)
            }
        }
        
        # Then control for all controlled cells together
        pastedControls = paste(signifCells2,collapse=",")
        if(EnrichmentMode=="Linear"){
            if(annotLevel!=controlledAnnotLevel){
                genesCovarData = read.table(genesCovarFile,stringsAsFactors = FALSE,header=TRUE)
                genesCovarData2 = merge(genesCovarData,controlledCovarCols[,c("entrez",signifCells2)])
                write.table(genesCovarData2,file=genesCovarFile,quote=FALSE,row.names=FALSE,sep="\t")
            }        
            sumstatsPrefix2 = sprintf("%s.level%s.%sUP.%sDOWN.ControlFor_%s",magmaPaths$filePathPrefix,annotLevel,upstream_kb,downstream_kb,pastedControls)
            magma_cmd = sprintf("magma --gene-results '%s.genes.raw' --gene-covar '%s' --model direction=pos condition='%s' --out '%s'",magmaPaths$filePathPrefix,genesCovarFile,pastedControls,sumstatsPrefix2)
        }else{
            controlledCovarCols2 = controlledCovarCols
            colnames(controlledCovarCols2)[-1] = sprintf("%s.covar",colnames(controlledCovarCols2)[-1])
            write.table(controlledCovarCols2,file=controlCovarFile,quote=FALSE,row.names=FALSE,sep="\t")
            controlledCTcovarNames = colnames(controlledCovarCols2)[-1]
            pastedControlCovars = paste(controlledCTcovarNames,collapse=",")
            
            sumstatsPrefix2 = sprintf("%s.level%s.%sUP.%sDOWN.Top10.ControlFor_%s",magmaPaths$filePathPrefix,annotLevel,upstream_kb,downstream_kb,pastedControls)
            magma_cmd = sprintf("magma --gene-results '%s.genes.raw' --set-annot '%s' --gene-covar '%s' --model direction=pos  condition=%s --out '%s'",magmaPaths$filePathPrefix,geneCovarFile,controlCovarFile,pastedControlCovars,sumstatsPrefix2)            
        }
        print(magma_cmd);system(magma_cmd)    
        cond_res = load.magma.results.file(path=sprintf("%s.gsa.out",sumstatsPrefix2),annotLevel,ctd,genesOutCOND=NA,EnrichmentMode=EnrichmentMode,ControlForCT=pastedControls)
        allRes = rbind(allRes,cond_res)
        
        # This line makes it so the baseline results are appended to the conditonal results
        ctAssocs[[annotLevel]]$results = rbind(ctAssocs[[annotLevel]]$results,allRes)
        #ctAssocs[[annotLevel]]$results = allRes
    }
    
    # Calculate total number of tests performed
    totalTests = 0
    for(annotLevel in 1:sum(names(ctAssocs)=="")){
        totalTests = totalTests + dim(ctAssocs[[annotLevel]]$results)[1]
    }
    ctAssocs$total_conditional_tests_performed = totalTests
    
    ctAssocs$gwas_sumstats_path = gwas_sumstats_path
    ctAssocs$analysis_name = analysis_name
    ctAssocs$upstream_kb = upstream_kb
    ctAssocs$downstream_kb = downstream_kb
    ctAssocs$genome_ref_path = genome_ref_path    
    
    return(ctAssocs)
}