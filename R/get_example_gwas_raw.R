#' Example GWAS: raw (pre-munged)
#'
#' Write example GWAS summary statistics to disk.
#'
#' @param save_path Path to save sum stats.
#' @param timeout How many seconds to wait before timeout.
#'
#' @return Path to sumstats
#'
#' @keywords internal
#' @importFrom utils download.file
#' @importFrom R.utils gunzip
get_example_gwas_raw <- function(storage_dir = tempdir(),
                                 trait = c(
                                     "prospective_memory",
                                     "fluid_intelligence"
                                 ),
                                 timeout = 60 * 5) {
    #### Check trait ####
    trait <- tolower(trait)[1]
    if (trait == "prospective_memory") {
        URL <- paste(
            "https://www.dropbox.com/s/j6mde051pl8k8vu",
            "20018.gwas.imputed_v3.both_sexes.tsv.bgz?dl=1",
            sep = "/"
        )
        study_name <- "20018.gwas.imputed_v3.both_sexes.tsv"
    } else if (trait == "fluid_intelligence") {
        URL <- paste(
            "https://www.dropbox.com/s/t3lrfj1id8133sx",
            "20016_irnt.gwas.imputed_v3.both_sexes.tsv.bgz?dl=1",
            sep="/"
        )
        study_name <- "20016_irnt.gwas.imputed_v3.both_sexes.tsv"
    } else {
        stop("trait must be 'prospective_memory' or 'fluid_intelligence'")
    }

    gwas_sumstats_path <- file.path(storage_dir, basename(URL))
    unzipped_path <- gsub(".bgz|.gz|[?]|dl=1", "", gwas_sumstats_path)
    if (file.exists(unzipped_path)) {
        message("Importing pe-existing file.")
    } else {
        message(paste("Downloading example GWAS:", trait))
        options(timeout = timeout)
        utils::download.file(
            url = URL,
            destfile = gwas_sumstats_path
        )
        message("Unzipping example GWAS.")
        R.utils::gunzip(
            gwas_sumstats_path,
            unzipped_path
        )
    }
    message(paste("File saved at:", unzipped_path))
    return(unzipped_path)
}
