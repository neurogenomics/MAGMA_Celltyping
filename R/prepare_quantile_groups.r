#' Prepare quantile groups for each celltype based on specificity
#'
#' Quantile groups are stored in an extra matrix ('quantiles')
#' in the returned CTD. This function also removes any genes
#' from the CTD data which are not 1:1 orthologs with the GWAS species.
#'
#' @param ctd Cell type data
#' @param specificity_species Species name relevant to the cell type data,
#' (e.g. "mouse" or "human").
#' @param gwas_species Species name relevant to the GWAS data, in almost all
#'  cases this will be "human"
#' @param bins How many bins should specificity be divided into?
#'
#' @return The ctd with additional quantiles matrix
#'
#' @examples
#' \dontrun{
#' ctd <- prepare_quantile_groups(ctd = ewceData::ctd())
#' }
#'
#' @import dplyr
#' @importFrom EWCE standardise_ctd
#' @importFrom methods as
#' @importFrom Matrix t
#' @export
prepare_quantile_groups <- function(ctd,
                                    specificity_species = "mouse",
                                    gwas_species = "human",
                                    bins = 40) {
    ### Account for changes in naming conventions across EWCE versions ####
    bin_columns_func <- if (packageVersion("EWCE") >= "1.0.0") {
        EWCE::bin_columns_into_quantiles
    } else {
        EWCE::bin.columns.into.quantiles
    }
    bin_specificity_func <- if (packageVersion("EWCE") >= "1.0.0") {
        EWCE::bin_specificity_into_quantiles
    } else {
        EWCE::bin.specificity.into.quantiles
    }
    #### Convert orthologs ####
    if (specificity_species != gwas_species) {
        ctd <- EWCE::standardise_ctd(
            ctd = ctd,
            dataset = NULL,
            species = specificity_species,
            drop_nonhuman_genes = TRUE
        )
    }

    normalise_mean_exp <- function(spcMatrix) {
        message("Computing linear normalised mean expression.")
        spcMatrix$mean_exp <- as(spcMatrix$mean_exp, "matrix")
        spcMatrix$linear_normalised_mean_exp <- Matrix::t(
            Matrix::t(spcMatrix$mean_exp) *
                (1 / colSums(spcMatrix$mean_exp))
        )
        return(spcMatrix)
    }

    bin_expression_into_quantiles <- function(spcMatrix, bins) {
        message("Computing expression quantiles.")
        spcMatrix$expr_quantiles <- as.matrix(apply(
            spcMatrix$linear_normalised_mean_exp, 2,
            FUN = bin_columns_func, bins = bins
        ))
        rownames(spcMatrix$expr_quantiles) <- rownames(
            spcMatrix$linear_normalised_mean_exp
        )
        return(spcMatrix)
    }
    use.distance.to.add.expression.level.info <- function(spcMatrix) {
        message("Computing specificity distance.")
        spcMatrix$specificity <- methods::as(spcMatrix$specificity, "matrix")
        spcMatrix$spec_dist <- spcMatrix$specificity
        for (ct in colnames(spcMatrix$expr_quantiles)) {
            resTab <- data.frame(
                spec = spcMatrix$specificity_quantiles[, ct],
                exp = spcMatrix$expr_quantiles[, ct],
                gene = rownames(spcMatrix$linear_normalised_mean_exp),
                check.names = FALSE
            )
            resTab$dist <- sqrt((max(resTab$spec) - resTab$spec)^2 +
                (max(resTab$exp) - resTab$exp)^2)
            spcMatrix$spec_dist[, ct] <- resTab$dist
        }
        spcMatrix$spec_dist <- methods::as(spcMatrix$spec_dist, "matrix")
        spcMatrix$spec_dist <- max(spcMatrix$spec_dist,
            na.rm = TRUE
        ) - spcMatrix$spec_dist
        return(spcMatrix)
    }
    bin.specificityDistance.into.quantiles <- function(spcMatrix) {
        message("Computing specificity distance quantiles.")
        spcMatrix$specDist_quantiles <- apply(spcMatrix$spec_dist, 2,
            FUN = bin_columns_func
        )
        rownames(spcMatrix$specDist_quantiles) <- rownames(spcMatrix$spec_dist)
        spcMatrix$specDist_quantiles <- methods::as(
            spcMatrix$specDist_quantiles, "matrix"
        )
        return(spcMatrix)
    }
    ctd <- lapply(ctd, normalise_mean_exp)
    ctd <- lapply(ctd, bin_specificity_func, bins)
    ctd <- lapply(ctd, use.distance.to.add.expression.level.info)
    ctd <- lapply(ctd, bin.specificityDistance.into.quantiles) 
    return(ctd)
}
