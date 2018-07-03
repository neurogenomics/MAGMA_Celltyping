#' Get CTD dendro
#'
#' Get all dendrogram features used for plotting a dendrogram of cell type
#'
#' @return List containing ddata and ordered_cells
#'
#' @examples
#' get.ctd.dendro(ctd,annotLevel=2)
#'
#' @import ggdendro
#' @import gridExtra
#' @export
get.ctd.dendro <- function(ctd,annotLevel){
  library(ggdendro)
  library(gridExtra)
  # Prepare dendrogram
  binned_file_dist <- dist(t(ctd[[annotLevel]]$quantiles)) # euclidean distances between the rows
  binned_file_dist_hclust <- hclust(binned_file_dist)
  ddata <- ggdendro::dendro_data(binned_file_dist_hclust, type="rectangle")
  ordered_cells <- as.character(ddata$labels$label)
  return(list(ddata=ddata,ordered_cells=ordered_cells))
}