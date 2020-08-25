#' MAGMA tileplot
#'
#' Used after merging results from multiple GWAS studies
#'
#' @param ctd Cell type data strucutre containing $specificity_quantiles
#' @param results Output from either calculate_celltype_associations() or calculate_conditional_celltype_associations()
#' @param fileTag String apprended to the names of the saved PDFs, i.e. the name of the celltype data file used
#' @param height Height of the output tileplot
#' @param width Width of the output tileplot
#' @param annotLevel Annotation level to plot the results for
#' @param output_path Location where the results should be plotted
#'
#' @examples
#' ctAssocs = merge_magma_results.r(ctAssoc1,ctAssoc2)
#'
#' @export
magma.tileplot <- function(ctd,results,height=13,width=4,annotLevel=1,fileTag="",output_path){
    # First, error checking of arguments
    #if(sum(!c("Celltype","GCOV_FILE","log10p","q","level") %in% colnames(results))>0){stop("results dataframe must contain 'Celltype', 'GCOV_FILE', 'log10p', 'level' and 'q' columns")}
    if(sum(!c("Celltype","GCOV_FILE","log10p","level") %in% colnames(results))>0){stop("results dataframe must contain 'Celltype', 'GCOV_FILE', 'log10p' and 'level' columns")}
    if(length(unique(results$GCOV_FILE))<2){stop("Must be more than one unique entry in results$GCOV_FILE for plotting tileplot")}
    if(!annotLevel %in% results$level){stop(sprintf("No results for annotation level = %s found in results",annotLevel))}
    #if(is.na(base_path)){stop("base_path must be specified. This is the location where tile plots are saved.")}
    
    
    # Reduce results to only contain results for the relevant annotation level
    results = results[results$level==annotLevel,]
    
    # Setup folder for saving figures
    magmaPaths = get.magma.paths(output_path=output_path)
    figurePath = magmaPaths$tiles
    
    # Then prep
    library(ggplot2)
    ctdDendro = get.ctd.dendro(ctd,annotLevel=annotLevel)
    
    # Order cells by dendrogram
    results$Celltype <- factor(results$Celltype, levels=ctdDendro$ordered_cells)
    # Plot it!
    results$q=p.adjust(results$P,method="bonferroni")
    
    # Prepare the tileplot
    fig_Heatmap_WOdendro = ggplot(results)+geom_tile(aes(x=GCOV_FILE,y=Celltype,fill=log10p)) + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
        theme(legend.position="none") +
        xlab("") + ylab("") + scale_fill_gradient(low = "darkblue",high = "white") + ggtitle("MAGMA")+
        geom_point(aes(x=GCOV_FILE,y=Celltype,size=ifelse(q<0.00001, "HUGEdot", ifelse(q<0.0001, "BIGdot", ifelse(q<0.001, "BiiGdot", ifelse(q<0.05, "dot", "no_dot"))))),col="black") +
        scale_size_manual(values=c(HUGEdot=4,BIGdot=3,BiiGdot=2,dot=1, no_dot=NA), guide="none") 
    
    # Prepare the dendrogram
    Fig_Dendro <- ggplot(segment(ctdDendro$ddata)) + geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) + coord_flip() +  theme_dendro()
    Fig_Dendro <- Fig_Dendro + scale_x_continuous(expand = c(0, 1.3)) 
    
    # Write the figures to PDF
    pdf(sprintf("%s/CombinedRes_TilePlot_MAGMA_noDendro_level%s_%s.pdf",figurePath,annotLevel,fileTag),width=width,height=height)
    print(fig_Heatmap_WOdendro)
    dev.off()
    
    pdf(sprintf("%s/CombinedRes_TilePlot_MAGMA_wtDendro_level%s_%s.pdf",figurePath,annotLevel,fileTag),width=width+1,height=height)
    print(grid.arrange(fig_Heatmap_WOdendro,Fig_Dendro,ncol=2,widths=c(0.8,0.2)))
    dev.off()
    
    return(list(heatmap=fig_Heatmap_WOdendro,dendro=Fig_Dendro))
}