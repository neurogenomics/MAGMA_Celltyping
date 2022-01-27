#' Check that sumstats has correct columns and that they are
#'  in the correct order for MAGMA and LDSC.
#'
#' @return col_headers The new column headers for the sumstats file
#'
#' @param path Filepath for the summary statistics file to be formatted.
#' 
#' @importFrom data.table fread fwrite
#' @keywords internal
format_sumstats_for_magma_macOnly <- function(path) {
    # stopper("This function is now depreciated. I stopped maintaining it in 
    # favour of the crossplatform version.")
    .Deprecated("MungeSumstats::format_sumstats") 
    # Test which OS is being used
    mungesumstats_deprecation_msg()
}
