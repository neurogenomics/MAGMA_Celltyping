choose_ewce_bin_function <- function() {
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
    return(bin_columns_func)
}
