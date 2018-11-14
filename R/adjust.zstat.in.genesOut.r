#' Adjust MAGMA Z-statistic from .genes.out files
#'
#' Used when you want to directly analyse the gene level z-scores corrected for gene length etc
#'
#' @param ctd Cell type data structure
#' @param magma_file A MAGMA .genes.out file
#' @param sctSpecies Either 'human' or 'mouse'
#'
#' @examples
#' magmaGenesOut = adjust.zstat.in.genesOut(ctd,magma_file="/Users/natske/GWAS_Summary_Statistics/MAGMA_Files/20016.assoc.tsv.10UP.1.5DOWN/20016.assoc.tsv.10UP.1.5DOWN.genes.out",sctSpecies="mouse")
#'
#' @export
adjust.zstat.in.genesOut <- function(ctd,magma_file="/Users/natske/GWAS_Summary_Statistics/MAGMA_Files/20016.assoc.tsv.10UP.1.5DOWN/20016.assoc.tsv.10UP.1.5DOWN.genes.out",sctSpecies="mouse"){
    allGenes = rownames(ctd[[1]]$specificity)
    
    # Get mouse-->human othologs with human entrez
    orth = One2One::ortholog_data_Mouse_Human$orthologs_one2one[,2:3]
    hgnc2entrez = all_hgnc_wtEntrez
    colnames(hgnc2entrez)=c("human.symbol","entrez")
    orth2 = merge(orth,hgnc2entrez,by="human.symbol")
    
    # Load the MAGMA data
    magma = read.table(magma_file,stringsAsFactors = FALSE,header=TRUE)
    magma$entrez = magma$GENE
    if(sctSpecies=="mouse"){
        magma = merge(magma, orth2,by="entrez")
    }
    magma = magma[order(magma$P),]
    if(sctSpecies=="mouse"){
        magma = magma[magma$mouse.symbol %in% allGenes,]
        magma = magma[!duplicated(magma$mouse.symbol),]
    }
    magma$Q = p.adjust(magma$P,method="bonferroni")
    magma$logNSNPS=log(magma$NSNPS)
    magma$logNPARAM=log(magma$NPARAM)
    magma = magma[!(magma$CHR==6 & magma$START>=25000000 & magma$STOP<=34000000),] # DROP MHC: chr6, 25-34 mb
    magma$GENELEN = abs(magma$STOP-magma$START)
    magma$logGENELEN = log(magma$GENELEN)
    
    # Regress out effects of NSNPS and NPARAM (see 'boxplots_by_decile.r' and the section on downsampling for info)
    #--- NSNPS only really has mjaor effects (i.e. zscore+2) when a gene has ~10000 SNPS
    mod = lm(ZSTAT~NSNPS+logNSNPS+NPARAM+logNPARAM+GENELEN+logGENELEN,data=magma)
    magma$ADJ_ZSTAT = magma$ZSTAT - (magma$NSNPS*mod$coefficients[2] + magma$logNSNPS*mod$coefficients[3] + 
                                         magma$NPARAM*mod$coefficients[4] + magma$logNPARAM*mod$coefficients[5] + 
                                         magma$NPARAM*mod$coefficients[6] + magma$logNPARAM*mod$coefficients[7]    )
    #magma = arrange(magma,desc(magma$ADJ_ZSTAT))
    #magma = magma[order(magma$ADJ_ZSTAT,decreasing=TRUE),]
    return(magma)
}