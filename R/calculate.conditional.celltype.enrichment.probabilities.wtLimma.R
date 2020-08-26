#' Calculate celltype enrichments without MAGMA using adjusted MAGMA Z-statistic from .genes.out files
#'
#' @param magma1 Output from adjust.zstat.in.genesOut() (Alzheimers for instance)
#' @param magma2 Output from adjust.zstat.in.genesOut() (EduYears for instance)
#' @param ctd Cell type data structure
#' @param thresh A threshold on low specificity values (used to drop genes)
#' @param sctSpecies Either 'human' or 'mouse'
#' @param annotLevel Annotation level of CellTypeData to use (integer)
#'
#' @examples
#' magmaGenesOut = adjust.zstat.in.genesOut(ctd,magma_file="/Users/natske/GWAS_Summary_Statistics/MAGMA_Files/20016.assoc.tsv.10UP.1.5DOWN/20016.assoc.tsv.10UP.1.5DOWN.genes.out",sctSpecies="mouse")
#'
#' @export
calculate.conditional.celltype.enrichment.probabilities.wtLimma <- function(magma1,magma2,ctd,thresh=0.0001,sctSpecies="mouse",annotLevel=1){
    library(limma)
    library(tidyverse)
    
    # Join the two MAGMA Z-score data frames
    m1 = magma1
    m2 = magma2
    library(tidyverse)
    shared = intersect(m1$entrez,m2$entrez)
    rownames(m1) = m1$entrez
    rownames(m2) = m2$entrez
    m1a = m1[as.character(shared),] %>% dplyr::select(entrez,ADJ_ZSTAT)
    m2a = m2[as.character(shared),] %>% dplyr::select(entrez,ADJ_ZSTAT)
    m3 = merge(m1a,m2a,by="entrez")
    
    # Regress the second from the first & get the residuals
    print(cor(m3$ADJ_ZSTAT.x,m3$ADJ_ZSTAT.y))
    mod=lm(data=m3,ADJ_ZSTAT.x~ADJ_ZSTAT.y)
    print(anova(mod))
    m3$zNew = residuals(mod)
    m3 = m3 %>% dplyr::rename(ADJ_original=ADJ_ZSTAT.y) %>% dplyr::rename(ADJ_ZSTAT.y=zNew)
    magma2_NEW = merge(m3[,c("entrez","ADJ_ZSTAT.y")] %>% dplyr::rename(ADJ_ZSTAT=ADJ_ZSTAT.y),magma2 %>% dplyr::select(entrez,mouse.symbol),by="entrez")
    
    # Melt it (so it's ready for mixed modelling)!
    m4 = melt(m3,id="entrez")
    m5 = merge(m4,m1[,c("entrez","mouse.symbol")],by="entrez")    
    magmaAdjZ=m5
    
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
        library(lme4)
        mZ = reshape::cast(magma_with_ct1,formula = mouse.symbol~variable,fun.aggregate = mean)
        mZ2 = merge(mZ,magma_with_ct1[,c("mouse.symbol","percentile")],by="mouse.symbol")
        mZ2$residualPercentile=residuals(lm(percentile~ADJ_ZSTAT.y,data=mZ2))
        
        mod.mod   = lmer(value ~ 1 + variable + percentile + variable * percentile + (1|mouse.symbol), data=unique(magma_with_ct1),REML=FALSE)
        mod.null  = lmer(value ~ 1 + variable + percentile +                         (1|mouse.symbol), data=unique(magma_with_ct1),REML=FALSE)
        
        modANOVA = anova(mod.mod,mod.null)
        
        # Convert p-value to one-sided
        ps[count] = modANOVA$`Pr(>Chisq)`[2]
        coef[count] = summary(mod.mod)$coefficients["variableADJ_ZSTAT.y:percentile",1]
    }
    
    # Get baseline results
    baseline1 = calculate.celltype.enrichment.probabilities.wtLimma(magmaAdjZ=magma1,ctd,thresh=thresh,sctSpecies=sctSpecies,annotLevel=annotLevel)
    baseline2 = calculate.celltype.enrichment.probabilities.wtLimma(magmaAdjZ=magma2_NEW,ctd,thresh=thresh,sctSpecies=sctSpecies,annotLevel=annotLevel)
    baselineRes = data.frame(ct=names(baseline1),p1_baseline=baseline1,p2_baseline=baseline2)
    
    output = list(ps=ps,coef=coef)
    df=data.frame(ct=names(output$ps),ps=output$ps,coef=output$coef)
    df2 = merge(df,baselineRes,by="ct")
    df2=df2[order(df2$coef),]
    df2$qs = p.adjust(df2$ps,method="BH")
    df3 = reshape::melt(df2,id.vars=c("ct","coef","ps","qs"))
    df3$direction = "No Change"
    df3$direction[df3$coef>0 & df3$qs<0.05] = "Increased Enrichment"
    df3$direction[df3$coef<0 & df3$qs<0.05] = "Decreased Enrichment"
    df3$log10p = log10(df3$value)
    
    library(cowplot)
    ggplot(df3)+geom_bar(aes(x=ct,y=log10p,fill=variable),stat="identity",position="dodge")+scale_y_reverse()+facet_wrap(~direction)+
         coord_flip() + ylab(expression('-log'[10]*'(pvalue)')) + xlab("") + theme_minimal_hgrid()
    
    # Now plot the baseline in one facet + significance of changes in the next
    df3b = df3
    #df3b[df3b$coef<0,]$qs = df3b[df3b$coef<0,]$qs*-1
    df4_a = df3b[,c("ct","qs","coef")]
    df4_a$log10p = log(df4_a$qs)
    df4_a[df4_a$coef<0,]$log10p = -1*df4_a[df4_a$coef<0,]$log10p
    df4_a = df4_a %>% dplyr::select(ct,qs,log10p)
    df4_a$variable = ""
    df4_a$type   = "Significance of Changes"
    
    df4_b = df3[,c("ct","value","log10p","variable")] %>% rename(qs=value)
    df4_b$type = "Baseline"
    
    df4 = rbind(df4_a,df4_b)
    ggplot(df4)+geom_bar(aes(x=ct,y=log10p,fill=variable),stat="identity",position="dodge")+scale_y_reverse()+facet_wrap(~type,scale="free_x")+
        coord_flip() + ylab(expression('-log'[10]*'(pvalue)')) + xlab("") + theme_minimal_hgrid()
    
    
    return(df2)
}
