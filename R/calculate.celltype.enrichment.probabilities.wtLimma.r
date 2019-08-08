#' Calculate celltype enrichments without MAGMA using adjusted MAGMA Z-statistic from .genes.out files
#'
#' @param magmaAdjZ Output from adjust.zstat.in.genesOut()
#' @param ctd Cell type data structure
#' @param thresh A threshold on low specificity values (used to drop genes)
#' @param sctSpecies Either 'human' or 'mouse'
#'
#' @examples
#' magmaGenesOut = adjust.zstat.in.genesOut(ctd,magma_file="/Users/natske/GWAS_Summary_Statistics/MAGMA_Files/20016.assoc.tsv.10UP.1.5DOWN/20016.assoc.tsv.10UP.1.5DOWN.genes.out",sctSpecies="mouse")
#'
#' @export
calculate.celltype.enrichment.probabilities.wtLimma <- function(magmaAdjZ,ctd,thresh=0.0001,sctSpecies="mouse",annotLevel=1){
    library(limma)
    
    # First get names of all cell types
    allCellTypes = colnames(ctd[[annotLevel]]$specificity)
    
    # Initialise variables
    ps = coef = rep(0,length(allCellTypes))
    names(ps) = names(coef) = allCellTypes
    
    # Loop over each celltype testing for enrichment
    count=0
    for(ct1 in allCellTypes){
        count=count+1
        print(ct1)
        if(sctSpecies=="mouse"){
            mgiS  = magmaAdjZ$mouse.symbol
        }else{
            mgiS = magmaAdjZ$human.symbol
        }
        props = ctd[[annotLevel]]$specificity[mgiS,ct1]
        notExp = rep(0,length(props))
        
        # Drop any genes with expression below threshold
        mgiS = mgiS[props>thresh]
        props = props[props>thresh]
        notExp[props<thresh]=-1
        
        # Determine which expression decile genes fall into
        quantiles = quantile(props[props>thresh],probs=seq(from=0,to=1,by=0.1))
        perc  = as.numeric(cut(props,quantiles,include.lowest=TRUE))
        perc[is.na(perc)] = 0
        
        # Merge decile groups with MAGMA zscores
        if(sctSpecies=="mouse"){
            geneGroups = data.frame(mgi_symbol=mgiS,proportion=props,percentile=perc)
            magma_with_ct1 = geneGroups %>%
                dplyr::rename(mouse.symbol=mgi_symbol) %>% merge(magmaAdjZ,by="mouse.symbol") %>%
                dplyr::filter(percentile>=0)
        }else{
            geneGroups = data.frame(hgnc_symbol=mgiS,proportion=props,percentile=perc)
            magma_with_ct1 = geneGroups %>%
                dplyr::rename(human.symbol=hgnc_symbol) %>% merge(magmaAdjZ,by="human.symbol") %>%
                dplyr::filter(percentile>=0)            
        }
        
        # Fit a linear model and get p,-value and coefficient (slope)
        expMat = matrix(0,nrow=1,ncol=dim(magma_with_ct1)[1])
        expMat[1,] = magma_with_ct1$ADJ_ZSTAT
        rownames(expMat) = c("actual")
        colnames(expMat) = magma_with_ct1$mouse.symbol
        dmat <- model.matrix(~ magma_with_ct1$percentile)
        fit <- lmFit(expMat, dmat)
        fit2 <- eBayes(fit)
        res <- topTable(fit2,number=1)
        res[res$logFC<0,"P.Value"]=1
        res[res$logFC>=0,"P.Value"]=res[res$logFC>=0,"P.Value"]/2   
        
        # Convert p-value to one-sided
        ps[count] = res$P.Value
    }
    return(ps)
}
