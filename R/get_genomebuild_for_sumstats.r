#' Get genome build for sumstats
#'
#' Compare the SNP:CHR:BP data against a dataset of all
#' Hapmap3 SNP:CHR:BP data for two genome builds (GRCh37 and GRCh38).
#' Evaluate which genome build matches the SNP locations from
#' the sum stats file.
#'
#' @param path Filepath of the summary statistics file
#'
#' @return Either 'GRCh37' or 'GRCh38'
#'
#' @examples
#' \dontrun{
#' path <- MAGMA.Celltyping::get_example_gwas()
#' genome_build <- MAGMA.Celltyping::get_genomebuild_for_sumstats(path = path)
#' }
#' @export
#' @importFrom utils read.table
get_genomebuild_for_sumstats <- function(path) {
    .Deprecated("MungeSumstats::format_sumstats")
    mungesumstats_deprecation_msg()
}
