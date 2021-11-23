bin_specificityDistance_into_quantiles <- function(spcMatrix,
                                                   verbose = FALSE) {
    messager("Computing specificity distance quantiles.", v = verbose)
    bin_columns_func <- choose_ewce_bin_function()
    
    spcMatrix$specDist_quantiles <- apply(
        spcMatrix$spec_dist, 2,
        FUN = bin_columns_func
    )
    rownames(spcMatrix$specDist_quantiles) <- rownames(spcMatrix$spec_dist)
    spcMatrix$specDist_quantiles <- methods::as(
        spcMatrix$specDist_quantiles, "matrix"
    )
    return(spcMatrix)
}
