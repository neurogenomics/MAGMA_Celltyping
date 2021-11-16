use_distance_to_add_expression_level_info <- function(ctd_1lvl,
                                                      verbose = FALSE) {
    messager("Computing specificity distance.", v = verbose)
    ctd_1lvl$specificity <- methods::as(ctd_1lvl$specificity, "matrix")
    ctd_1lvl$spec_dist <- ctd_1lvl$specificity
    for (ct in colnames(ctd_1lvl$expr_quantiles)) {
        resTab <- data.frame(
            spec = ctd_1lvl$specificity_quantiles[, ct],
            exp = ctd_1lvl$expr_quantiles[, ct],
            gene = rownames(ctd_1lvl$linear_normalised_mean_exp),
            check.names = FALSE,
            check.rows = FALSE
        )
        resTab$dist <- sqrt((max(resTab$spec) - resTab$spec)^2 +
            (max(resTab$exp) - resTab$exp)^2)
        ctd_1lvl$spec_dist[, ct] <- resTab$dist
    }
    ctd_1lvl$spec_dist <- methods::as(ctd_1lvl$spec_dist, "matrix")
    ctd_1lvl$spec_dist <- max(ctd_1lvl$spec_dist,
        na.rm = TRUE
    ) - ctd_1lvl$spec_dist
    return(ctd_1lvl)
}
