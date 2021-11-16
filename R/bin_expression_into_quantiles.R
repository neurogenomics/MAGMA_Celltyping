bin_expression_into_quantiles <- function(spcMatrix,
                                          bins,
                                          verbose = TRUE) {
    messager("Computing expression quantiles.", v = verbose)
    spcMatrix$expr_quantiles <- as.matrix(apply(
        spcMatrix$linear_normalised_mean_exp, 2,
        FUN = bin_columns_func, bins = bins
    ))
    rownames(spcMatrix$expr_quantiles) <- rownames(
        spcMatrix$linear_normalised_mean_exp
    )
    return(spcMatrix)
}
