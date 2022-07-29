#' Prepare quantile groups for each celltype based on specificity
#'
#' Quantile groups are stored in an extra matrix ('quantiles')
#' in the returned CTD. This function also removes any genes
#' from the CTD data which are not 1:1 orthologs with the GWAS species.
#'
#' @param standardise Whether to run \link[EWCE]{standardise_ctd} first.
#' Provides gene ortholog conversion.
#' @param ... Additional arguments passed to \link[EWCE]{standardise_ctd}. 
#' @inheritParams EWCE::standardise_ctd
#'
#' @return The ctd converted to \code{output_species} gene symbols
#' with additional quantiles matrix.
#'
#' @examples
#' ctd_orig <- ewceData::ctd()
#' ctd <- MAGMA.Celltyping::prepare_quantile_groups(ctd = ctd_orig)
#' @importFrom EWCE standardise_ctd
#' @importFrom methods as
#' @importFrom Matrix t
#' @export
prepare_quantile_groups <- function(ctd,
                                    standardise = TRUE,
                                    non121_strategy = "drop_both_species",
                                    input_species = "mouse",
                                    output_species = "human",
                                    numberOfBins = 40,
                                    verbose = TRUE,
                                    ...) {

    #### Convert orthologs ####
    if (isTRUE(standardise)) {
        ctd <- EWCE::standardise_ctd(
            ctd = ctd,
            dataset = NULL,
            non121_strategy = non121_strategy,
            input_species = input_species,
            output_species = output_species,
            numberOfBins = numberOfBins,
            verbose = verbose, 
            ...
        )
    }
    #### Compute specificity quantiles (if necessary) ####
    if(!"specificity_quantiles" %in% names(ctd[[1]])){
        ctd <- lapply(ctd, EWCE::bin_specificity_into_quantiles,
                      numberOfBins = numberOfBins, 
                      matrix_name = "specificity_quantiles") 
    }
    #### Compute specificity deciles ####
    ctd <- lapply(ctd, EWCE::bin_specificity_into_quantiles,
                  numberOfBins = 10, 
                  matrix_name = "specificity_deciles") 
    ### Check that the number of quantiles in each col == numberOfBins ####
    quantiles_counts <- check_quantiles(ctd = ctd, 
                                        matrix_name = "specificity_quantiles", 
                                        numberOfBins = numberOfBins,
                                        metric = "n")
    deciles_counts <- check_quantiles(ctd = ctd, 
                                      matrix_name = "specificity_deciles", 
                                      numberOfBins = 10,
                                      metric = "n")
    return(ctd)
}
