#' Chooses EWCE function 
#' 
#' Identifies the name of the EWCE binning 
#' function to use depending on which version 
#' of \pkg{EWCE} is currently installed.
#' 
#' @returns Function.
#' 
#' @keywords internal
choose_ewce_bin_function <- function(func = "bin_columns_into_quantiles") {
    requireNamespace("EWCE")
    requireNamespace("utils")
    if(func == "bin_columns_into_quantiles"){
        bin_columns_func <- if (utils::packageVersion("EWCE") >= "1.0.0") {
            EWCE::bin_columns_into_quantiles
        } else {
            EWCE::bin.columns.into.quantiles
        }
        return(bin_columns_func)
    }
    if(func == "bin_specificity_into_quantiles"){
        bin_specificity_func <- if (utils::packageVersion("EWCE") >= "1.0.0") {
            EWCE::bin_specificity_into_quantiles
        } else {
            EWCE::bin.specificity.into.quantiles
        }
        return(bin_specificity_func)
    }
}
