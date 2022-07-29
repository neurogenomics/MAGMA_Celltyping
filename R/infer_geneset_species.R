#' Infer gene set species
#' 
#' Infers species from from a gene set
#' using \link[orthogene]{infer_species}. 
#' If \code{geneset_species} is not \code{NULL}, 
#' this will be returned instead of inferring the species. 
#' 
#' @inheritParams infer_ctd_species
#' @inheritParams calculate_geneset_enrichment 
#' @inheritDotParams orthogene::infer_species
#' @returns Inferred species name.
#' 
#' @export 
#' @importFrom orthogene infer_species
#' @examples   
#' geneset <- MAGMA.Celltyping::rbfox_binding
#' geneset_species <- infer_geneset_species(geneset = geneset)
infer_geneset_species <- function(geneset,
                                  geneset_species = NULL,
                                  verbose = 1,
                                  ...){
    #### Mainly for adjust_zstat_in_genesOut where ctd is optional ####
    if(is.null(geneset)) return(NULL)
    #### Infer ####
    if(is.null(geneset_species)) {
        messager("geneset_species=NULL: Inferring species from gene names.",
                 v=sum(verbose)>0)
        geneset_species <- orthogene::infer_species(
            gene_df = geneset,
            verbose = sum(verbose)>1,
            make_plot = FALSE,
            ...
        )$top_match
        messager("Inferred gene set species:",geneset_species,
                 v=sum(verbose)>0)
    }
    return(geneset_species)
}
