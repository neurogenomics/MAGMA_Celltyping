normalise_mean_exp <- function(ctd_1lvl,
                               verbose = FALSE) {
    messager("Computing linear normalised mean expression.", v = verbose)
    ctd_1lvl$mean_exp <- as(ctd_1lvl$mean_exp, "matrix")
    ctd_1lvl$linear_normalised_mean_exp <- Matrix::t(
        Matrix::t(ctd_1lvl$mean_exp) *
            (1 / colSums(ctd_1lvl$mean_exp))
    )
    return(ctd_1lvl)
}
