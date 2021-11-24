bin_specificityDistance_into_quantiles <- function(spcMatrix,
                                                   verbose = FALSE) {
    messager("Computing specificity distance quantiles.", v = verbose)  
    spcMatrix$specDist_quantiles <- apply(
        spcMatrix$spec_dist,
        2,
        FUN = EWCE::bin_columns_into_quantiles
    )
    rownames(spcMatrix$specDist_quantiles) <- rownames(spcMatrix$spec_dist)
    spcMatrix$specDist_quantiles <- methods::as(
        spcMatrix$specDist_quantiles, "matrix"
    )
    return(spcMatrix)
}
