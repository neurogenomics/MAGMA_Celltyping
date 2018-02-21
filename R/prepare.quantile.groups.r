#' Prepare quantile groups for each celltype based on specificity
#'
#' Quantile groups are stored in an extra matrix ('quantiles') in the returned CTD. This function also removes any genes
#' from the CTD data which are not 1:1 orthologs with the GWAS species.
#'
#' @param ctd Cell type data
#' @param specificity_species Species name relevant to the cell type data, i.e. "mouse" or "human"
#' @param gwas_species Species name relevant to the GWAS data, in almost all cases this will be "human"
#' @param numberOfBins How many bins should specificity be divided into?
#'
#' @return The ctd with additional quantiles matrix
#'
#' @examples
#' ctd2 = lapply(ctd,filter.by.orthologs,one2one_ortholog_symbols = ortholog_data[,2])
#'
#' @import tibble
#' @import dplyr
#' @export
prepare.quantile.groups <- function(ctd,specificity_species="mouse",gwas_species="human",numberOfBins=41){
    library(tibble)
    # First drop all genes without 1:1 homologs
    if(specificity_species != gwas_species){
        print("Dropping all genes that do not have 1:1 homologs between the two species")
        allHomologs = load.homologs()
        ortholog_data = analyse.orthology(specificity_species,gwas_species,allHomologs)$orthologs_one2one
        ctd = lapply(ctd,filter.by.orthologs,one2one_ortholog_symbols = ortholog_data[,2])
    }
    
    # Quantiles will be stored within the CTD as 'quantiles'
    bin.columns.into.quantiles <- function(spcValues){
        quantileValues = rep(0,length(spcValues))
        quantileValues[spcValues>0] = as.numeric(cut(spcValues[spcValues>0], 
                                                     breaks=quantile(spcValues[spcValues>0], probs=seq(0,1, by=1/numberOfBins), na.rm=TRUE), 
                                                     include.lowest=TRUE))
        return(quantileValues)
    }
    bin.specificity.into.quantiles <- function(spcMatrix){
        spcMatrix$quantiles = apply(spcMatrix$specificity,2,FUN=bin.columns.into.quantiles)
        rownames(spcMatrix$quantiles) = rownames(spcMatrix$specificity)
        return(spcMatrix)
    }
    ctd = lapply(ctd,bin.specificity.into.quantiles)
    
    return(ctd)
}