#' Filter CellTypeDataset
#' 
#' Filter CellTypeDataset (CTD) 
#' specificity and mean_exp from CTD list to only keep a subset of genes.
#'
#' Created so that lapply can be used to do this.
#'
#' @param ctd_oneLevel A single level of a CTD
#' @param one2one_ortholog_symbols character expansion for the text.
#'
#' @returns The CTD with both specificity and mean
#'  expression matrices filtered to only contain 1:1 ortholog genes.
#'
#' @source
#' \code{
#' ctd <- ewceData::ctd()
#' orths <- ewceData::mouse_to_human_homologs()
#' ctd2 <- lapply(ctd, filter_by_orthologs, orths$MGI.symbol)
#' }
#' 
#' @keywords internal 
filter_by_orthologs <- filter.by.orthologs <- function(
    ctd_oneLevel,
    one2one_ortholog_symbols) {
    .Deprecated() 
    ctd_oneLevel$specificity <- 
        ctd_oneLevel$specificity[
            rownames(ctd_oneLevel$specificity) %in% one2one_ortholog_symbols, ]
    ctd_oneLevel$mean_exp <- 
        ctd_oneLevel$mean_exp[
            rownames(ctd_oneLevel$mean_exp) %in% one2one_ortholog_symbols, ]
    return(ctd_oneLevel)
}
