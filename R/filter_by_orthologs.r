#' Filter specificity and mean_exp from CTD list to only keep a subset of genes
#'
#' Created so that lapply can be used to do this
#'
#' @param ctd_oneLevel method to order colors (\code{"hsv"} or \code{"cluster"})
#' @param one2one_ortholog_symbols character expansion for the text
#'
#' @return The ctd with both specificity and mean expression matrices filtered to only contain 1:1 ortholog genes
#'
#' @examples
#' ctd2 = lapply(ctd,filter.by.orthologs,one2one_ortholog_symbols = ortholog_data[,2])
filter_by_orthologs <- function(ctd_oneLevel,
                                one2one_ortholog_symbols){
    #ctd_oneLevel$specificity = ctd_oneLevel$specificity[rownames(ctd_oneLevel$specificity) %in% ortholog_data[,2],]
    #ctd_oneLevel$mean_exp = ctd_oneLevel$mean_exp[rownames(ctd_oneLevel$mean_exp) %in% ortholog_data[,2],]
    ctd_oneLevel$specificity = ctd_oneLevel$specificity[rownames(ctd_oneLevel$specificity) %in% one2one_ortholog_symbols,]
    ctd_oneLevel$mean_exp = ctd_oneLevel$mean_exp[rownames(ctd_oneLevel$mean_exp) %in% one2one_ortholog_symbols,]
    return(ctd_oneLevel)
}