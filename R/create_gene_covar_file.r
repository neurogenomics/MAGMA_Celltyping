#' Create gene covar file
#'
#' The gene covar file is the input to MAGMA for the celltype association analysis. This code was functonalised because it is called by both baseline and conditional analysis.
#'
#' @param genesOutFile The output of the second call to MAGMA (performed in the map.snps.to.genes function)
#' @param ctd Cell type data structure. Must contain quantiles.
#' @param annotLevel Annot level for which the gene covar file should be constructed
#' @param specificity_species
#' @param genesOutCOND
#'
#' @return Filepath for the gene covar file
#'
#' @examples
#' genesCovarFilePath = create_gene_covar_file(genesOutFile,ctd)
#'
#' @export
create_gene_covar_file <- function(genesOutFile,ctd,annotLevel,specificity_species,genesOutCOND=NA){
    # Check specificity_species
    if(!specificity_species %in% c("human","mouse")){stop("Specificity species must be either 'human' or 'mouse'")}
    
    # Read in the genes.out file (which has a p-value for each entrez gene)
    genesOut = read.table(genesOutFile,stringsAsFactors = FALSE)
    data(all_hgnc_wtEntrez)
    colnames(all_hgnc_wtEntrez)[1] = "human.symbol"
    
    # Find which HGNC genes are in genesOut
    #genesWithGWAS = all_hgnc_wtEntrez[all_hgnc_wtEntrez$entrezgene %in% genesOut$V1,]
    
    # Find all genes expressed in CTD which have 1:1 orthologs
    #orth = ortholog_data_Mouse_Human$orthologs_one2one
    #genesInCTD = orth[orth$mouse.symbol %in% rownames(ctd[[1]]$specificity),]$
    
    # How many HGNC symbols are in CTD, are 1:1 orthologs, and are in genesOut
    #sum(genesInCTD$human.symbol %in% genesWithGWAS$human.symbol) # 12834
        
    if(specificity_species=="mouse"){
        data(ortholog_data_Mouse_Human)
        
        # Because sumstats use entrez genes & ctd uses gene symbols, match entrez-->symbols
        entrez_mgi = merge(all_hgnc_wtEntrez,ortholog_data_Mouse_Human$orthologs_one2one[,2:3],by="human.symbol")
        entrez_mgi = entrez_mgi[!is.na(entrez_mgi$entrezgene),]
        entrez_mgi = entrez_mgi[entrez_mgi$mouse.symbol %in% rownames(ctd[[annotLevel]]$quantiles),]
        
        # Get the quantiles from ctd and put into correct format, using entrez symbols
        #quantDat = ctd[[annotLevel]]$quantiles[all_hgnc_wtEntrez$human.symbol,]
        quantDat = ctd[[annotLevel]]$quantiles[entrez_mgi$mouse.symbol,]
        quantDat2 = suppressWarnings(data.frame(entrez=entrez_mgi$entrezgene,quantDat))
        quantDat2 = quantDat2[!duplicated(quantDat2$entrez),]
    }
    
    if(specificity_species=="human"){
        # Get the quantiles from ctd and put into correct format, using entrez symbols
        humanSymsPresent = all_hgnc_wtEntrez$human.symbol[all_hgnc_wtEntrez$human.symbol %in% rownames(ctd[[annotLevel]]$quantiles)]
        entrezTable = all_hgnc_wtEntrez[all_hgnc_wtEntrez$human.symbol %in% humanSymsPresent,]
        quantDat = ctd[[annotLevel]]$quantiles[entrezTable$human.symbol,]
        quantDat2 = suppressWarnings(data.frame(entrez=entrezTable$entrez,quantDat))
        quantDat2 = quantDat2[!duplicated(quantDat2$entrez),]
    }
    
    # 
    if(!is.na(genesOutCOND)){
        genesOutCOND_data = read.table(file=genesOutCOND,stringsAsFactors = FALSE)
        colnames(genesOutCOND_data) = genesOutCOND_data[1,]
        genesOutCOND_data = genesOutCOND_data[-1,c("GENE","ZSTAT")]
        colnames(genesOutCOND_data)[1]="entrezgene"
        
        # Expand the entrez definitions to include other entrez symbols matching the relevant gene symbols
        genesOutCOND_data2 = merge(all_hgnc_wtEntrez,genesOutCOND_data,by="entrezgene")[,c(1,3)]
        colnames(genesOutCOND_data2)[1]="entrez"
        
        #quantDat2new = merge(quantDat2,genesOutCOND_data2,by="entrez")
        quantDat2 = merge(quantDat2,genesOutCOND_data2,by="entrez")
    }
    
    # Write genes covar file to disk
    geneCovarFile=tempfile()
    write.table(quantDat2,file=geneCovarFile,quote=FALSE,row.names=FALSE,sep="\t")
    return(geneCovarFile)
}