#' Create gene sets 
#' 
#' Support function for \link[MAGMA.Celltyping]{get_driver_genes}.
#' 
#' @param res_input Enrichment results output from 
#' \link[MAGMA.Celltyping]{calculate_celltype_enrichment_limma}. 
#' @param n_genes Max number of genes to return. 
#' @param spec_deciles Which specificity deciles to use
#'  from the CellTypeDataset.
#' @param human.symbol Whether to use human gene symbols. 
#' 
#' @keywords internal
create_genesets <- function(res_input,
                            n_genes = 100,
                            spec_deciles = 10,
                            human.symbol = TRUE) {
    
    specificity_decile <- Celltype_id <-ADJ_ZSTAT <-  NULL;
    symbol_species <- if (human.symbol) "human.symbol" else "mouse.symbol"
    message("+ Returning ", symbol_species)
    res_input <- subset(res_input, specificity_decile %in% spec_deciles)
    genesets <- lapply(unique(res_input$Celltype_id), function(x) {
        g <- (subset(res_input, Celltype_id == x) %>%
            dplyr::slice_max(ADJ_ZSTAT,
                             n = n_genes,
                             with_ties = FALSE))[[symbol_species]]
        message("   ", x, " : ", length(g), " genes")
        return(g)
    }) %>% `names<-`(unique(res_input$Celltype_id))
    return(genesets)
}
