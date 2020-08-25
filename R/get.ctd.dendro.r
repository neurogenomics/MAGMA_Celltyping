#' Get CTD dendro
#'
#' Get all dendrogram features used for plotting a dendrogram of cell type
#'
#' @return List containing ddata and ordered_cells
#'
#' @examples
#' get.ctd.dendro(ctd,annotLevel=2)
#' @param ctd Cell type data strucutre containing $specificity_quantiles
#' @param annotLevel Annot level for which the gene covar file should be constructed
#' @importFrom ggdendro dendro_data
#' @importFrom ggdendro theme_dendro
# @import gridExtra
#' @import ggplot2
#' @importFrom stats dist
#' @importFrom stats hclust
#' @importFrom ggdendro segment
#' @export
get.ctd.dendro <- function(ctd,annotLevel){
  #library(ggdendro)
  #library(gridExtra)
  # Prepare dendrogram
  binned_file_dist <- stats::dist(t(ctd[[annotLevel]]$specificity_quantiles)) # euclidean distances between the rows
  binned_file_dist_hclust <- stats::hclust(binned_file_dist)
  ddata <- ggdendro::dendro_data(binned_file_dist_hclust, type="rectangle")
  ordered_cells <- as.character(ddata$labels$label)
  dendroPlot <- ggplot2::ggplot(ggdendro::segment(ddata)) + ggplot2::geom_segment(aes_string(x="x", y="y", xend="xend", yend="yend")) + ggplot2::coord_flip() +  ggdendro::theme_dendro() + ggplot2::scale_x_continuous(expand = c(0, 1.3)) 
  return(list(ddata=ddata,ordered_cells=ordered_cells,dendroPlot=dendroPlot))
}