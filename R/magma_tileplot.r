#' MAGMA tile plot
#'
#' Used after merging results from multiple GWAS studies. 
#' @param ctd Cell type data structure containing "specificity_quantiles".
#' @param results Output from either 
#' \link[MAGMA.Celltyping]{calculate_celltype_associations} or
#' \link[MAGMA.Celltyping]{calculate_conditional_celltype_associations}.
#' @param fileTag String appended to the names of the saved PDFs,
#'  i.e. the name of the celltype data file used.
#' @param height Height of the output tileplot.
#' @param width Width of the output tileplot.
#' @param annotLevel Annotation level to plot the results for.
#' @param qvalue_thresh The multiple-testing corrected p-value at 
#' which to filter results (set to \code{NULL} to skip filtering).
#' @param output_path Location where the results should be plotted.
#' @param fill Variable in \code{results} to fill the plot tile colors by.
#' Passed to \link[ggplot2]{aes_string}.
#' @param wrap_width Width at which to wrap x-axis text labels.
#' @param verbose Print messages.
#' @inheritParams plot_celltype_associations
#' @return List of two ggplot objects.
#' 
#' @export 
#' @importFrom stats p.adjust 
#' @importFrom methods show
#' @examples 
#' res <- MAGMA.Celltyping::enrichment_results[[1]]
#' results <- res$ctAssocMerged$level1$results
#' ctd <- ewceData::ctd()
#' tile_res <- magma_tileplot(ctd=ctd, results=results)
magma_tileplot <- function(ctd,
                           results, 
                           height = 13, 
                           width = 4, 
                           annotLevel = 1,
                           fill = "-log10(P)",
                           fileTag = "", 
                           output_path = tempdir(),
                           qvalue_thresh = NULL,
                           plotDendro = TRUE,
                           show_plot = TRUE,
                           bind_plots = TRUE,
                           wrap_width = 50,
                           verbose = TRUE) {
    
    # devoptera::args2vars(magma_tileplot)
    requireNamespace("ggplot2")
    requireNamespace("ggdendro")
    requireNamespace("patchwork")
    
    #### First, error checking of arguments ####
    if (sum(!c("Celltype", "GCOV_FILE", "log10p", "level") %in%
            colnames(results)) > 0) {
        stopper("results dataframe must contain:",
        "'Celltype', 'GCOV_FILE', 'log10p' and 'level' columns")
    }
    if (length(unique(results$GCOV_FILE)) < 2) {
        stopper("Must be more than one unique entry in",
                "results$GCOV_FILE for plotting tileplot")
    }
    if (!annotLevel %in% results$level) {
        stopper(sprintf("No results for annotation level = %s found in results",
                        annotLevel))
    } 
    # Reduce results to only contain results for the relevant annotation level
    results <- results[results$level == annotLevel, ]
    #### Setup folder for saving figures ####
    fig_dir <- get_magma_paths(output_path = output_path)$tiles 
    #### Then prep #### 
    results$Celltype <- fix_celltype_names2(results$Celltype,
                                            make_unique = FALSE)
    #### Add dendrogram ####
    if(isTRUE(plotDendro)){
        ctdDendro <- get_ctd_dendro(ctd = ctd, 
                                    annotLevel = annotLevel,
                                    verbose = verbose) 
        ctdDendro$ordered_cells <- fix_celltype_names2(ctdDendro$ordered_cells, 
                                                       make_unique = FALSE)
        #### Order cells by dendrogram ####
        results$Celltype <- factor(x = results$Celltype, 
                                   levels = ctdDendro$ordered_cells, 
                                   ordered = TRUE)
    }
    #### Apply multiple-testing correction ####
    results$q <- stats::p.adjust(results$P, method = "bonferroni")
    results$dotsize <- cut(x = results$q, 
                           breaks = c(0,0.00001,0.0001,0.001,0.05,1),
                           labels = c(4,3,2,1,NA)) 
    if(!is.null(qvalue_thresh)){
        results <- subset(results,q<=qvalue_thresh)
    } 
    #### Prepare the tileplot ####
    results$GCOV_FILE <- stringr::str_wrap(gsub("\\.",". ",results$GCOV_FILE),
                                           width = wrap_width)
    fig_Heatmap_WOdendro <- ggplot2::ggplot(results) +
        ggplot2::geom_tile(
            ggplot2::aes_string(x = "GCOV_FILE", 
                                y = "Celltype", 
                                fill = fill
                         )) + 
        ggplot2::labs(x="MAGMA file",
                      y="Cell type",
                      title = "MAGMA enrichment results") + 
        ggplot2::scale_fill_gradient(low = "white", high = "darkblue") +
        ggplot2::geom_point(
            ggplot2::aes_string(x = "GCOV_FILE", y = "Celltype", 
                                size = "dotsize"), 
            col = "black", 
            na.rm = TRUE) +
        ggplot2::theme_bw() +
        ggplot2::theme( 
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), 
            legend.position = "top", 
            legend.direction = "horizontal")  
    #### Bind plots together ####
    if(isTRUE(bind_plots)){
        plt <- patchwork::wrap_plots(fig_Heatmap_WOdendro, 
                                     ctdDendro$dendroPlot,
                                     ncol = 2) 
    } else {
        plt <- fig_Heatmap_WOdendro
    } 
    ##### Write the figures to PDF ####
    if(!is.null(fig_dir)){
        filename <- file.path(
            fig_dir, 
            paste0("CombinedRes_TilePlot_MAGMA_wtDendro_level",
                   annotLevel,"_",fileTag,".pdf")) 
        messager("Saving plot ==>",filename,v=verbose)
        ggplot2::ggsave(filename = filename,
                        plot = plt,
                        width = width, 
                        height = height)
        if(isFALSE(bind_plots)){
            filename <- file.path(
                fig_dir, 
                paste0("CombinedRes_TilePlot_MAGMA_Dendro_level",
                       annotLevel,"_",fileTag,".pdf")) 
            messager("Saving plot ==>",filename,v=verbose)
            ggplot2::ggsave(filename = filename,
                            plot = ctdDendro$dendroPlot,
                            width = width, 
                            height = height)
        }
    } 
    #### Show plot ####
    if(isTRUE(show_plot)){
        if(isTRUE(bind_plots)) {
            methods::show(plt)
        } else {
            methods::show(fig_Heatmap_WOdendro)
        }
    }
    #### Return ####
    if(isTRUE(bind_plots)){
       return(plt)
    } else {
        return(list(heatmap = fig_Heatmap_WOdendro, 
                    dendro = ctdDendro$dendroPlot))
    }
}

magma.tileplot <- function(...){
    .Deprecated("magma_tileplot")
    magma_tileplot(...)
}
