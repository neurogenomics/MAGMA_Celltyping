#' Infer CellTypeDataset species
#' 
#' Infers species from from level 1 of a CellTypeDataset
#' using \link[orthogene]{infer_species}. 
#' If \code{ctd_species} is not \code{NULL}, 
#' this will be returned instead of inferring the species. 
#' 
#' @param verbose Message verbosity. 
#' \itemize{
#' \item{\code{0} or \code{FALSE} : }{
#' Don't print any messages.
#' }
#' \item{\code{1} or \code{TRUE} : }{
#' Only print messages from \pkg{MAGMA.Celltyping}.
#' }
#' \item{\code{2} or \code{c(TRUE,TRUE)} : }{
#' Print messages from \pkg{MAGMA.Celltyping} and 
#' the internal \pkg{orthogene} function.}
#' } 
#' @inheritParams celltype_associations_pipeline
#' @inheritDotParams orthogene::infer_species
#' @returns Inferred species name.
#' 
#' @export 
#' @importFrom orthogene infer_species
#' @examples   
#' ctd_species <- infer_ctd_species(ctd = ewceData::ctd())
infer_ctd_species <- function(ctd, 
                              ctd_species = NULL,
                              verbose = 1,
                              ...){
    #### Mainly for adjust_zstat_in_genesOut where ctd is optional ####
    if(is.null(ctd)) return(NULL)
    #### Infer ####
    if(is.null(ctd_species)) {
        messager("ctd_species=NULL: Inferring species from gene names.",
                 v=sum(verbose)>0)
        ctd_species <- orthogene::infer_species(
            gene_df = ctd[[1]]$mean_exp,
            verbose = sum(verbose)>1,
            make_plot = FALSE,
            ...
        )$top_match
        messager("Inferred ctd species:",ctd_species,
                 v=sum(verbose)>0)
    }
    return(ctd_species)
}
