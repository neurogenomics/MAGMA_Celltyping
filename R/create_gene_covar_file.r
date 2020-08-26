#' Create gene covar file
#'
#' The gene covar file is the input to MAGMA for the celltype association analysis. This code was functonalised because it is called by both baseline and conditional analysis.
#'
#' @param genesOutFile The output of the second call to MAGMA (performed in the map.snps.to.genes function)
#' @param ctd Cell type data structure. Must contain quantiles.
#' @param annotLevel Annot level for which the gene covar file should be constructed
#' @param specificity_species Species name relevant to the cell type data, i.e. "mouse" or "human"
#' @param genesOutCOND [Optional] Path to a genes.out file to condition on. Used if you want to condition on a different GWAS.
#'
#' @return Filepath for the gene covar file
#'
#' @examples
#' genesCovarFilePath = create_gene_covar_file(genesOutFile,ctd)
#'
#' @export
#' @importFrom utils read.table
create_gene_covar_file <- function(genesOutFile,ctd,annotLevel,specificity_species,genesOutCOND=NA){
    quantDat2 = map_specificity_to_entrez(ctd=ctd,annotLevel=annotLevel,specificity_species=specificity_species)
    #colnames(quantDat2)[2:length(colnames(quantDat2))] = colnames(ctd[[controlledAnnotLevel]]$quantiles)
    
    if(dim(quantDat2)[1]<100){stop("Less than one hundred genes detected after mapping genes between species. Was specificity_species defined correctly?")}
    
    # Read in the genes.out file (which has a p-value for each entrez gene)
    # genesOut = read.table(genesOutFile,stringsAsFactors = FALSE)
    
    # If the analysis is being run conditionally on another GWAS
    if(!is.na(genesOutCOND[1])){
        for(i in 1:length(genesOutCOND)){
            genesOutCOND_data = read.table(file=genesOutCOND[i],stringsAsFactors = FALSE)
            colnames(genesOutCOND_data) = genesOutCOND_data[1,]
            genesOutCOND_data = genesOutCOND_data[-1,c("GENE","ZSTAT")]
            colnames(genesOutCOND_data)[1]="entrezgene"
            
            # Expand the entrez definitions to include other entrez symbols matching the relevant gene symbols
            data(all_hgnc_wtEntrez);    colnames(all_hgnc_wtEntrez)[1] = "human.symbol"        
            genesOutCOND_data2 = merge(all_hgnc_wtEntrez,genesOutCOND_data,by="entrezgene")[,c(1,3)]
            colnames(genesOutCOND_data2)[1]="entrez"
            colnames(genesOutCOND_data2)[2]=sprintf("ZSTAT%s",i)
            
            #quantDat2new = merge(quantDat2,genesOutCOND_data2,by="entrez")
            quantDat2 = merge(quantDat2,genesOutCOND_data2,by="entrez")
        }
    }
    
    # Write genes covar file to disk
    geneCovarFile=tempfile()
    write.table(quantDat2,file=geneCovarFile,quote=FALSE,row.names=FALSE,sep="\t")
    return(geneCovarFile)
}