bin_specificity <- function(ctd_1lvl,
                            numberOfBins = 40,
                            defaultBin = as.integer(numberOfBins / 2),
                            new_matrix_name = "expr_quantiles",
                            check_quantiles = TRUE,
                            verbose = FALSE) {
    messager("Computing specificity distance quantiles.", v = verbose)
    bin_specificity_func <- choose_ewce_bin_function()
    ctd_1lvl[[new_matrix_name]] <- apply(ctd_1lvl$linear_normalised_mean_exp,
        2,
        FUN = bin_specificity_func,
        numberOfBins,
        defaultBin
    )
    rownames(ctd_1lvl[[new_matrix_name]]) <- rownames(ctd_1lvl$mean_exp)
    ctd_1lvl[[new_matrix_name]] <- methods::as(
        ctd_1lvl[[new_matrix_name]], "matrix"
    )
    return(ctd_1lvl)
}
