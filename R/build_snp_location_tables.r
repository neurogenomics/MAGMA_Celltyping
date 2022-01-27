#' Build SNP location tables
#'
#' Build data table with RSID / CHR / BP / GenomeBuild
#' for all Hapmap3 SNPs in GRCh37 and GRCh38.
#'
#' @return Save SNP_LOC_DATA to package data
#' 
#' @keywords internal
build_snp_location_tables <- function() {
    .Deprecated("MungeSumstats::get_genome_builds")
    msg <- paste0(
        "FUNCTION DEPRECATED: Our lab have created a robust ",
        "bioconductor package for ",
        "formatting multiple types of summary\nstatistics files: ",
        "MungeSumstats. MungeSumstats includes functionality to check ",
        "a summary statistics file's genome\nbuild. We strongly ",
        "advise using MungeSumstats instead!"
    )
    stop(msg)
}
