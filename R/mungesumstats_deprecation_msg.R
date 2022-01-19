#' Deprecation notice 
#' 
#' Deprecation notice for functions now performed by \pkg{MungeSumstats}.
#' @param error Whether to return an error or simply a message.
#' 
#' @keywords internal
mungesumstats_deprecation_msg <- function(error=TRUE){
    msg <- paste0(
        "FUNCTION DEPRECATED: Our lab have created a robust ",
        "bioconductor package for ",
        "formatting multiple types of summary\nstatistics files: ",
        "MungeSumstats. The function MungeSumstats::format_sumstats ",
        "will perform better than\nformat.sumstats.for.magma as it can",
        " handle more types of summary statistics files and issues and",
        " also offers\nmore flexibility to the user. We strongly adv",
        "ise using it instead!"
    )
    if(error) stop(msg) else message(msg)
}