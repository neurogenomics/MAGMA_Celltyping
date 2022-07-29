#' MAGMA tileplot
#'
#' Used after merging results from multiple GWAS studies.
#'
#' @param ctd Cell type data structure containing "specificity_quantiles".
#' @param results Output from either 
#' \link[MAGMA.Celltyping]{calculate_celltype_associations} or
#' \link[MAGMA.Celltyping]{calculate_conditional_celltype_associations}.
#' @param fileTag String appended to the names of the saved PDFs,
#'  i.e. the name of the celltype data file used.
#' @param height Height of the output tileplot.
#' @param width Width of the output tileplot.
#' @param annotLevel Annotation level to plot the results for.
#' @param output_path Location where the results should be plotted.
#' 
#' @return List of two ggplot objects.
#' 
#' @export 
#' @importFrom stats p.adjust 
#' @importFrom methods show
magma_tileplot <- function(ctd,
                           results, 
                           height = 13, 
                           width = 4, 
                           annotLevel = 1,
                           fileTag = "", 
                           output_path = tempdir()) {
    requireNamespace("ggplot2")
    requireNamespace("ggdendro")
    requireNamespace("grDevices")
    requireNamespace("gridExtra")
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

    # Setup folder for saving figures
    magmaPaths <- get_magma_paths(output_path = output_path)
    figurePath <- magmaPaths$tiles

    # Then prep
    ctdDendro <- get_ctd_dendro(ctd, annotLevel = annotLevel)

    # Order cells by dendrogram
    results$Celltype <- factor(results$Celltype, 
                               levels = ctdDendro$ordered_cells)
    # Plot it!
    results$q <- stats::p.adjust(results$P, method = "bonferroni")
    results$dotsize <- cut(x = results$q, 
                           breaks = c(0,0.00001,0.0001,0.001,0.05,1),
                           labels = c(4,3,2,1,NA)) 

    # Prepare the tileplot
    fig_Heatmap_WOdendro <- ggplot2::ggplot(results) +
        ggplot2::geom_tile(
            ggplot2::aes_string(x = "GCOV_FILE", 
                                y = "Celltype", fill = "log10p")) +
        ggplot2::theme(axis.text.x = 
                           ggplot2::element_text(angle = 90, hjust = 1)) +
        ggplot2::theme(legend.position = "none") +
        ggplot2::xlab("") +
        ggplot2::ylab("") +
        ggplot2::scale_fill_gradient(low = "darkblue", high = "white") +
        ggplot2::ggtitle("MAGMA") +
        ggplot2::geom_point(
            ggplot2::aes_string(x = "GCOV_FILE", y = "Celltype", 
                                size = "dotsize"), 
            col = "black") 

    #### Prepare the dendrogram ####
    Fig_Dendro <- ggplot2::ggplot(ggdendro::segment(ctdDendro$ddata)) +
        ggplot2::geom_segment(
            ggplot2::aes_string(x = "x", y = "y",
                                xend = "xend", yend = "yend")) +
        ggplot2::coord_flip() +
        ggdendro::theme_dendro()
    Fig_Dendro <- Fig_Dendro + ggplot2::scale_x_continuous(expand = c(0, 1.3))

    ##### Write the figures to PDF ####
    grDevices::pdf(
        sprintf("%s/CombinedRes_TilePlot_MAGMA_noDendro_level%s_%s.pdf", 
                figurePath, annotLevel, fileTag),
        width = width, height = height)
    methods::show(fig_Heatmap_WOdendro)
    grDevices::dev.off()

    grDevices::pdf(
        sprintf("%s/CombinedRes_TilePlot_MAGMA_wtDendro_level%s_%s.pdf", 
                figurePath, annotLevel, fileTag),
        width = width + 1, height = height)
    methods::show(gridExtra::grid.arrange(fig_Heatmap_WOdendro,
                                  Fig_Dendro, 
                                  ncol = 2, 
                                  widths = c(0.8, 0.2)))
    grDevices::dev.off()

    return(list(heatmap = fig_Heatmap_WOdendro, 
                dendro = Fig_Dendro))
}

magma.tileplot <- function(...){
    .Deprecated("magma_tileplot")
    magma_tileplot(...)
}
