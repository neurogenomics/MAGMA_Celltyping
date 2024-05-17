#' Format GWAS summary statistics
#' 
#' Check that sumstats has correct columns and that they are
#'  in the correct order for MAGMA and LDSC.
#'
#' @param path Filepath for the summary statistics file to be formatted.
#' 
#' @return col_headers The new column headers for the sumstats file
#'
#' @examples 
#' \dontrun{
#' path <- MAGMA.Celltyping::get_example_gwas()
#' new_path <- MAGMA.Celltyping:::format.sumstats.for.magma(path)
#' }
#' 
#' @export
#' @importFrom data.table fread fwrite setcolorder 
#' @importFrom utils read.table 
format_sumstats_for_magma <- function(path) {
    .Deprecated("MungeSumstats::format_sumstats")
    mungesumstats_deprecation_msg()
}


format.sumstats.for.magma <- function(path){
    format_sumstats_for_magma(path)
}
