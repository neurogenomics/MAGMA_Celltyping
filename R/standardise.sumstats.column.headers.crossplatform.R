#' Standardise the column headers in the Summary Statistics files
#'  (CROSSPLATFORM)
#'
#' Use a reference data table of common column header names
#'  (stored in sumstatsColHeaders.rda) convert them to a standard set,
#'   i.e. chromosome --> CHR
#'
#' This function does not check that all the required column headers are present
#'
#' The amended header is written directly back into the file
#'
#' @param first_line String containing the first line of the sumstats file
#'
#' @returns The amended column headers
#'  (also the column headers will be written directly
#'   into the summary statistics file)
#'
#' @examples
#' \dontrun{
#' path <- MAGMA.Celltyping::get_example_gwas()
#' first_line <- readLines(path)[1]
#' col_headers <- MAGMA.Celltyping:::standardise.sumstats.column.headers.crossplatform(
#'      first_line = first_line)
#' }
#' @keywords internal
standardise.sumstats.column.headers.crossplatform <- function(first_line) {
    .Deprecated("MungeSumstats::format_sumstats")
    mungesumstats_deprecation_msg()
}
 
