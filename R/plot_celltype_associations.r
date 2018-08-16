#' Plot celltype associations calculated using MAGMA
#'
#' Can take input from either calculate_celltype_associations() or calculate_conditional_celltype_associations()
#'
#' @param ctAssocs Output from either calculate_celltype_associations() or calculate_conditional_celltype_associations()
#' @param ctd Cell type data strucutre containing $quantiles (required to calculate dendrogram)
#' @param useSignificanceLine TRUE or FALSE. Should their be a vertical line marking bonferroni signifiance? 
#' @param savePDF TRUE or FALSE. Save figure to file or print to screen?
#' @param fileTag String apprended to the names of the saved PDFs, i.e. the name of the celltype data file used
#'
#' @return NULL
#'
#' @examples
#' ctAssocs = calculate_celltype_associations(ctd,gwas_sumstats_path)
#'
#' @import ggplot2
#' @import cowplot
#' @export
plot_celltype_associations <- function(ctAssocs,ctd,useSignificanceLine=TRUE,savePDF=TRUE,fileTag="",plotDendro=TRUE,gwas_title="",plotLegend=TRUE){
    # CHECK: THAT RESULTS FOR ONLY ONE GWAS WERE PROVIDED (for more than one use magma.tileplot.r)
    whichGWAS = unique(gsub("DOWN\\..*","DOWN",unique(ctAssocs[[1]]$results$GCOV_FILE)))
    if(length(whichGWAS)>1){stop("Only results for one GWAS at a tile should be provided to plot_celltype_association. For multiple GWAS, use magma.tileplot()")}
    
    magmaPaths = get.magma.paths(ctAssocs$gwas_sumstats_path,ctAssocs$upstream_kb,ctAssocs$downstream_kb)
    
    # CHECK: WAS A TITLE PROVIDED FOR THE PLOT?
    if(gwas_title==""){gwas_title=whichGWAS}
    
    # CHECK: THAT A MINIMAL SET OF COLUMN HEADERS ARE INCLUDED IN THE RESULTS TABLE
    requiredHeaders = c("Celltype","P","log10p","Method","EnrichmentMode","CONTROL","CONTROL_label")
    
    # Is the analysis top10%, linear or merged?
    print(ctAssocs[[1]]$results)
    print(unique(ctAssocs[[1]]$results$EnrichmentMode))
    if(length(unique(ctAssocs[[1]]$results$EnrichmentMode))==1){
        if(unique(ctAssocs[[1]]$results$EnrichmentMode)=="Linear"){
            analysisType = "Linear"
        }else{
            analysisType = "TopDecile"
        }
    }else{  analysisType = "Merged" }
    
    # Generate the plots (for each annotation level seperately)
    library(cowplot)
    figures = list()
    for(annotLevel in 1:sum(names(ctAssocs)=="")){
        # SET: NEW COLUMN COMBINING METHODS or ENRICHMENTTYPES
        ctAssocs[[annotLevel]]$results$FullMethod = sprintf("%s %s",ctAssocs[[annotLevel]]$results$Method,ctAssocs[[annotLevel]]$results$EnrichmentMode)
        
        if(plotDendro==TRUE){
            # Order cells by dendrogram
            ctdDendro = get.ctd.dendro(ctd,annotLevel=annotLevel)
            ctAssocs[[annotLevel]]$results$Celltype <- factor(ctAssocs[[annotLevel]]$results$Celltype, levels=ctdDendro$ordered_cells)
        }
        
        a2 <- ggplot(ctAssocs[[annotLevel]]$results, aes(x = factor(Celltype), y = log10p, fill=FullMethod)) + scale_y_reverse()+geom_bar(stat = "identity",position="dodge") + coord_flip() + ylab(expression('-log'[10]*'(pvalue)')) + xlab("")
        a2 <- a2 + theme(legend.position = c(0.5, 0.8)) + ggtitle(gwas_title) + theme(legend.title=element_blank())
        if(plotLegend==FALSE){    a2 = a2 + theme(legend.position="none") }
        if(useSignificanceLine){  a2 = a2+geom_hline(yintercept=log(as.numeric(0.05/ctAssocs$total_baseline_tests_performed),10),colour="black")    }
        theFig = a2
        
        # If the results come from a BASELINE analysis... 
        if(length(unique(ctAssocs[[1]]$results$CONTROL))==1){
            
            if(plotDendro==TRUE){     theFig = grid.arrange(a2,ctdDendro$dendroPlot,ncol=2,widths=c(0.8,0.2))        }
            
            if(savePDF){
                fName = sprintf("%s/%s.%sUP.%sDOWN.annotLevel%s.Baseline.%s.%s.pdf",magmaPaths$figs,magmaPaths$gwasFileName,ctAssocs$upstream_kb,ctAssocs$downstream_kb,annotLevel,fileTag,analysisType)
                print("here")
                pdf(file=fName,width=10,height=1+2*(dim(ctAssocs[[annotLevel]]$results)[1]/10))
                    print(grid.arrange(a2,ctdDendro$dendroPlot,ncol=2,widths=c(0.8,0.2)))
                dev.off()
            }else{print(theFig)}            
        # IF THE RESULTS COME FROM A CONDITIONAL ANALYSIS    
        }else{
            theFig = theFig + facet_wrap(~CONTROL_label)
            
            if(savePDF){
                fName = sprintf("%s/%s.%sUP.%sDOWN.annotLevel%s.ConditionalFacets.%s.%s.pdf",magmaPaths$figs,magmaPaths$gwasFileName,ctAssocs$upstream_kb,ctAssocs$downstream_kb,annotLevel,fileTag,analysisType)
                pdf(file=fName,width=25,height=1+2*(dim(ctAssocs[[annotLevel]]$results)[1]/30))
                    print(theFig)
                dev.off()
            }else{print(theFig)}            
        }
        figures[[length(figures)+1]] = theFig
    }
    return(figures)
}