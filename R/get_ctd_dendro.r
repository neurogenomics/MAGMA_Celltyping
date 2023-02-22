#' Get CTD dendro
#'
#' Get all dendrogram features used for plotting a dendrogram of cell types.
#' @param ctd Cell type data structure containing \code{specificity_quantiles}.
#' @param annotLevel Annot level for which the gene covar file should
#'  be constructed.
#' @param verbose Print messages.
#' @inheritParams ggplot2::scale_x_continuous
#' @inheritDotParams prepare_quantile_groups
#' @return List containing ddata and ordered_cells
#'
#' @source 
#' \code{
#' MAGMA.Celltyping:::get_ctd_dendro(ctd = ctd, annotLevel = 1)
#' } 
#' @keywords internal
#' @importFrom stats dist hclust 
#' @importFrom ggdendro segment dendro_data theme_dendro
#' @importFrom Matrix t
get_ctd_dendro <- function(ctd,
                           annotLevel,
                           expand = c(0, 0.66),
                           verbose=TRUE,
                           ...) {  
    
    requireNamespace("ggplot2")
    
    if(!"specificity_quantiles" %in% names(ctd[[annotLevel]])){
        messager(
            "WARNING: specificity_quantiles matrix is missing from the ctd.",
            "Computing new specificity quantiles with 40 bins.",v=verbose
            )
        ctd <- prepare_quantile_groups(ctd = ctd, 
                                       verbose = verbose, 
                                       ...)
    }
    # Prepare dendrogram
    # euclidean distances between the rows
    binned_file_dist <- stats::dist(
        Matrix::t(ctd[[annotLevel]]$specificity_quantiles)
    ) 
    binned_file_dist_hclust <- stats::hclust(binned_file_dist)
    ddata <- ggdendro::dendro_data(binned_file_dist_hclust,
                                   type = "rectangle")
    ordered_cells <- as.character(ddata$labels$label)
    dendroPlot <- ggplot2::ggplot(ggdendro::segment(ddata)) +
        ggplot2::geom_segment(
            ggplot2::aes_string(x = "x", y = "y",
                                xend = "xend", yend = "yend")) +
        ggplot2::coord_flip() +
        ggdendro::theme_dendro() 
    if(!is.null(expand)){
        dendroPlot <- dendroPlot + 
            ggplot2::scale_x_continuous(expand = expand)
    }
    return(list(ddata = ddata,
                ordered_cells = ordered_cells, 
                dendroPlot = dendroPlot))
}

get.ctd.dendro <- function(...){
    .Deprecated("get_ctd_dendro")
    get_ctd_dendro(...)
}
