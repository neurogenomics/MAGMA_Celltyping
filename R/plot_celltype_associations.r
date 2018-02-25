#' Plot celltype associations calculated using MAGMA
#'
#' Can take input from either calculate_celltype_associations() or calculate_conditional_celltype_associations()
#'
#' @param ctAssocs Output from either calculate_celltype_associations() or calculate_conditional_celltype_associations()
#' @param useSignificanceLine TRUE or FALSE. Should their be a vertical line marking bonferroni signifiance? 
#' @param savePDF TRUE or FALSE. Save figure to file or print to screen?
#'
#' @return NULL
#'
#' @examples
#' ctAssocs = calculate_celltype_associations(ctd,gwas_sumstats_path)
#'
#' @import ggplot2
#' @import cowplot
#' @export
plot_celltype_associations <- function(ctAssocs,useSignificanceLine=TRUE,savePDF=TRUE){
    # Generate the plots (for each annotation level seperately)
    for(annotLevel in 1:sum(names(ctAssocs)=="")){
        if(length(unique(ctAssocs[[1]]$results$CONTROL))==1){
            theFig = ggplot(data=ctAssocs[[annotLevel]]$results)+geom_bar(aes(y=P,x=COVAR),stat="identity")+scale_y_log10()+coord_flip()+
                ylab(expression(-log[10](pvalue))) + xlab("Cell type")
            
            if(useSignificanceLine){
                theFig = theFig+geom_hline(yintercept=as.numeric(0.05/ctAssocs$total_baseline_tests_performed),colour="black")
            }
            
            if(savePDF){
                fName = sprintf("%s.%sUP.%sDOWN.%s.annotLevel%s.Baseline.pdf",ctAssocs$gwas_sumstats_path,ctAssocs$upstream_kb,ctAssocs$downstream_kb,ctAssocs$analysis_name,annotLevel)
                pdf(file=fName,width=10,height=1+2*(dim(ctAssocs[[annotLevel]]$results)[1]/10))
                print(theFig)
                dev.off()
            }else{
                print(theFig)
            }
        }else{
            theFig = ggplot(data=ctCondAssocs[[annotLevel]]$results)+geom_bar(aes(y=P,x=COVAR,fill=COVAR),stat="identity")+scale_y_log10()+coord_flip()+facet_wrap(~CONTROL_label)+
                ylab(expression(-log[10](pvalue))) + xlab("") + theme(legend.position="none")
            
            if(useSignificanceLine){
                theFig = theFig+geom_hline(yintercept=as.numeric(0.05/ctAssocs$total_conditional_tests_performed),colour="black")
            }
            
            if(savePDF){
                fName = sprintf("%s.%sUP.%sDOWN.%s.annotLevel%s.ConditionalFacets.pdf",ctAssocs$gwas_sumstats_path,ctAssocs$upstream_kb,ctAssocs$downstream_kb,ctAssocs$analysis_name,annotLevel)
                pdf(file=fName,width=25,height=1+2*(dim(ctCondAssocs[[annotLevel]]$results)[1]/30))
                print(theFig)
                dev.off()
            }else{
                print(theFig)
            }            
        }
    }
}