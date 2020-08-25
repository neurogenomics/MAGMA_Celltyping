#' Calculate celltype enrichments without MAGMA using adjusted MAGMA Z-statistic from .genes.out files
#'
#' @param magmaAdjZ Output from adjust.zstat.in.genesOut()
#' @param ctd Cell type data structure
#' @param thresh A threshold on low specificity values (used to drop genes)
#' @param sctSpecies Either 'human' or 'mouse'
#'
#' @examples
#' # The package stores an example genesOut file, so save this to a tempfile
#' myGenesOut = tempfile()
#' data.table::fwrite(MAGMA.Celltyping::genesOut,sep="\t",file=myGenesOut)
#' magmaGenesOut = adjust.zstat.in.genesOut(EWCE::ctd,magma_file=myGenesOut,sctSpecies="mouse")
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr rename
#' @importFrom dplyr filter
#' @importFrom limma lmFit
#' @importFrom limma eBayes
#' @importFrom limma topTable
#' @importFrom stats quantile
#' @importFrom rlang .data
calculate.celltype.enrichment.probabilities.wtLimma <- function(magmaAdjZ,ctd,thresh=0.0001,sctSpecies="mouse"){
    
    if("hgnc_symbol" %in% colnames(magmaAdjZ)){
        magmaAdjZ = magmaAdjZ %>% dplyr::rename(human.symbol=.data$hgnc_symbol)
    }
    
    # First get names of all cell types
    allCellTypes = colnames(ctd[[1]]$specificity)
    
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
        props = ctd[[1]]$specificity[mgiS,ct1]
        notExp = rep(0,length(props))
        
        # Drop any genes with expression below threshold
        mgiS = mgiS[props>thresh]
        props = props[props>thresh]
        notExp[props<thresh]=-1
        
        # Determine which expression decile genes fall into
        quantiles = stats::quantile(props[props>thresh],probs=seq(from=0,to=1,by=0.1))
        perc  = as.numeric(cut(props,quantiles,include.lowest=TRUE))
        perc[is.na(perc)] = 0
        
        # Merge decile groups with MAGMA zscores
        if(sctSpecies=="mouse"){
            geneGroups = data.frame(mgi_symbol=mgiS,proportion=props,percentile=perc)
            magma_with_ct1 = geneGroups %>%
                dplyr::rename(mouse.symbol=.data$mgi_symbol) %>% merge(magmaAdjZ,by="mouse.symbol") %>%
                dplyr::filter(.data$percentile>=0)
        }else{
            geneGroups = data.frame(hgnc_symbol=mgiS,proportion=props,percentile=perc)
            magma_with_ct1 = geneGroups %>%
                dplyr::rename(human.symbol=.data$hgnc_symbol) %>% merge(magmaAdjZ,by="human.symbol") %>%
                dplyr::filter(.data$percentile>=0)            
        }
        
        # Fit a linear model and get p,-value and coefficient (slope)
        expMat = matrix(0,nrow=1,ncol=dim(magma_with_ct1)[1])
        expMat[1,] = magma_with_ct1$ADJ_ZSTAT
        rownames(expMat) = c("actual")
        colnames(expMat) = magma_with_ct1$mouse.symbol
        dmat <- stats::model.matrix(~ magma_with_ct1$percentile)
        fit <- limma::lmFit(expMat, dmat)
        fit2 <- limma::eBayes(fit)
        res <- limma::topTable(fit2,number=1)
        res[res$logFC<0,"P.Value"]=1
        res[res$logFC>=0,"P.Value"]=res[res$logFC>=0,"P.Value"]/2   
        
        # Convert p-value to one-sided
        ps[count] = res$P.Value
    }
    return(ps)
}
