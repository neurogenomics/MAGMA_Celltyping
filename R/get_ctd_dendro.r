#' Get CTD dendro
#'
#' Get all dendrogram features used for plotting a dendrogram of cell types.
#'
#' @param ctd Cell type data strucutre containing \code{specificity_quantiles}.
#' @param annotLevel Annot level for which the gene covar file should
#'  be constructed.
#'
#' @return List containing ddata and ordered_cells
#'
#' @source 
#' \code{
#' MAGMA.Celltyping:::get_ctd_dendro(ctd = ctd, annotLevel = 1)
#' } 
#' @keywords internal
#' @import ggplot2
#' @importFrom stats dist hclust 
#' @importFrom ggdendro segment dendro_data theme_dendro
#' @importFrom Matrix t
get_ctd_dendro <- function(ctd,
                           annotLevel) {  
    # Prepare dendrogram
    # euclidean distances between the rows
    binned_file_dist <- stats::dist(
        Matrix::t(ctd[[annotLevel]]$specificity_quantiles)
    ) 
    binned_file_dist_hclust <- stats::hclust(binned_file_dist)
    ddata <- ggdendro::dendro_data(binned_file_dist_hclust, type = "rectangle")
    ordered_cells <- as.character(ddata$labels$label)
    dendroPlot <- ggplot2::ggplot(ggdendro::segment(ddata)) +
        ggplot2::geom_segment(ggplot2::aes_string(x = "x", y = "y",
                                         xend = "xend", yend = "yend")) +
        ggplot2::coord_flip() +
        ggdendro::theme_dendro() +
        ggplot2::scale_x_continuous(expand = c(0, 1.3))
    return(list(ddata = ddata,
                ordered_cells = ordered_cells, 
                dendroPlot = dendroPlot))
}

get.ctd.dendro <- function(...){
    .Deprecated("get_ctd_dendro")
    get_ctd_dendro(...)
}
