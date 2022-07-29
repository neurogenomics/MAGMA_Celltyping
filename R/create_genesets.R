#' Create gene sets 
#' 
#' Support function for \link[MAGMA.Celltyping]{get_driver_genes}.
#' 
#' @param res_input Enrichment results output from 
#' \link[MAGMA.Celltyping]{calculate_celltype_enrichment_limma}. 
#' @param n_genes Max number of genes to return. 
#' @param spec_deciles [Optional] Which specificity deciles to use
#'  from the CellTypeDataset.
#' @param gene_col Column containing gene symbols.
#' @param verbose Print messages.
#' 
#' @keywords internal
#' @importFrom data.table frank := setkey
create_genesets <- function(res_input,
                            n_genes = 100,
                            spec_deciles = NULL,
                            gene_col = "hgnc_symbol",
                            verbose = TRUE) {
    
    specificity_decile <- Celltype_id <-   NULL; 
    
    if(!gene_col %in% colnames(res_input)){
        stopper("gene_col not in res_input.")
    }
    messager("Identifying top driver genes per cell-type association:") 
    if(!is.null(spec_deciles)){
        res_input <- subset(res_input, specificity_decile %in% spec_deciles)   
    }
    genesets <- lapply(unique(res_input$Celltype_id), 
                       function(ct) {
           ADJ_ZSTAT <- specificity_proportion <- mean_rank <- NULL;
           ## Compute mean rank based on 
           ## GWAS metric (ADJ_ZSTAT) and CTD metric ()
           # "To sort by a column in descending order prefix "-"" 
            res <- res_input[(Celltype_id==ct),][,
                eval(c("rank1","rank2")):=list(
                data.table::frank(-ADJ_ZSTAT),
                data.table::frank(-specificity_proportion)
            ),] 
            res$mean_rank <- rowMeans(res[,c("rank1","rank2")])
            data.table::setkey(res,mean_rank)
        g <- res$hgnc_symbol[seq_len(min(nrow(res),n_genes))]
        messager("  ", ct,":",length(g), "driver genes")
        return(g)
    }) |> `names<-`(unique(res_input$Celltype_id))
    return(genesets)
}
