#' Create gene covar file
#'
#' The gene covar file is the input to MAGMA for the celltype association analysis. This code was functonalised 
#' because it is called by both baseline and conditional analysis.
#'
#' @param genesOutFile The output of the second call to MAGMA (performed in the map.snps.to.genes function)
#' @param ctd Cell type data structure. Must contain quantiles.
#' @param annotLevel Annot level for which the gene covar file should be constructed
#' @param specificity_species Species name relevant to the cell type data, i.e. "mouse" or "human"
#'
#' @return Filepath for the gene covar file
#'
#' @examples
#' library(MAGMA.Celltyping)
#' genesOutFile=
#' "/Users/natske/Naomi_Wray_Conditional/Results/Conditional/scz_adj_all_withaut_gsmr_bxy_aric_reference.raw"
#' ctd = prepare.quantile.groups(EWCE::ctd,specificity_species="mouse",numberOfBins=40)
#' geneSetsFilePath = create_top10percent_genesets_file(genesOutFile="",ctd=ctd,annotLevel=1,
#' specificity_species="mouse")
#'
#' @export
create_top10percent_genesets_file <- function(genesOutFile,ctd,annotLevel,specificity_species){
    ctd2 = prepare.quantile.groups(ctd,specificity_species=specificity_species,numberOfBins=10)
    quantDat2 = map_specificity_to_entrez(genesOutFile,ctd2,annotLevel,specificity_species)
    
    if(dim(quantDat2)[1]<100){stop("Less than one hundred genes detected after mapping genes between species. Was specificity_species defined correctly?")}
    
    cts = setdiff(colnames(quantDat2),"entrez")
    ctRows = rep("",length(cts))
    names(ctRows) = cts
    for(ct in cts){
        ctRows[ct] = paste(c(ct,quantDat2[quantDat2[,ct]==10,1]),collapse=" ")
    }
    
    # Write genes covar file to disk
    geneCovarFile=tempfile()
    #write.table(quantDat2,file=geneCovarFile,quote=FALSE,row.names=FALSE,sep="\t")
    write.table(ctRows,file=geneCovarFile,quote=FALSE,row.names=FALSE,sep="\t",col.names=FALSE)
    return(geneCovarFile)
}