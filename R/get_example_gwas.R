#' Import GWAS summary statistics
#'
#' Write example GWAS summary statistics to disk.
#' All examples originally come from the
#' \href{https://www.ukbiobank.ac.uk/}{UK Biobank}.
#' To reduce file size, SNPs have been filtered to Minor Allele Frequency (MAF)
#' > 5% and a nominal p-value < 0.05. However, in practice we
#'  recommend using full GWAS summary statistics
#'   (after applying \link[MungeSumstats]{format_sumstats}).
#'
#' @param trait Which trait to get GWAS summary stats for.
#' @param munged Whether to download the raw or pre-munged
#'  version of each GWAS (\emph{Default:} \code{TRUE}).
#' @param storage_dir Folder in which to store the GWAS summary stats.
#' @inheritParams get_example_gwas_raw
#' 
#' @source
#' \code{
#' #### fluid_intelligence ####
#' gwas_sumstats_path <- MAGMA.Celltyping::get_example_gwas(
#'     trait = "fluid_intelligence", munged = FALSE)
#' path_formatted <- MungeSumstats::format_sumstats(
#'     path=gwas_sumstats_path,
#'     save_path = tempfile(fileext = ".formatted.tsv.gz"),
#'     ref_genome ="GRCh37")
#' ss <- data.table::fread(path_formatted)
#' ss2 <- ss[MINOR_AF>=.05 & P<.05,]
#' data.table::fwrite(ss2, path_formatted, sep = "\t")
#' piggyback::pb_upload(file = path_formatted,
#'                      name = "fluid_intelligence.ukb.tsv.gz",
#'                      repo = "neurogenomics/MAGMA_Celltyping",
#'                      overwrite = TRUE)
#'
#' #### prospective_memory ####
#' gwas_sumstats_path <- MAGMA.Celltyping::get_example_gwas(
#'     trait = "prospective_memory", munged = FALSE)
#' path_formatted <- MungeSumstats::format_sumstats(
#'     path=gwas_sumstats_path,
#'     save_path = tempfile(fileext = ".formatted.tsv.gz"),
#'     ref_genome ="GRCh37")
#' ss <- data.table::fread(path_formatted)
#' ss2 <- ss[MINOR_AF>=.05 & P<.05,]
#' data.table::fwrite(ss2, path_formatted, sep = "\t")
#' piggyback::pb_upload(file = path_formatted,
#'                      name = "prospective_memory.ukb.tsv.gz",
#'                      repo = "neurogenomics/MAGMA_Celltyping",
#'                      overwrite = TRUE)

#' }
#' @return Path to downloaded GWAS summary statistics.
#'
#' @examples 
#' path_formatted <- MAGMA.Celltyping::get_example_gwas()
#' @export
#' @importFrom data.table fwrite
#' @importFrom tools R_user_dir
get_example_gwas <- function(trait = c(
                                 "educational_attainment",
                                 "fluid_intelligence",
                                 "prospective_memory"
                             ),
                             munged = TRUE,
                             storage_dir = tools::R_user_dir(
                                 package = "MAGMA.Celltyping",
                                 which = "cache"
                             ),
                             timeout = 60 * 5) {
    trait <- tolower(trait)[1]
    if (munged) {
        if (!trait %in% c(
            "fluid_intelligence",
            "prospective_memory",
            "educational_attainment"
        )) {
            stop("trait must be one of: 'prospective_memory',
                 'fluid_intelligence', or 'educational_attainment'")
        }
        message(paste("Importing munged GWAS summary statistics:", trait))
        if (trait == "educational_attainment") {
            unzipped_path <- file.path(
                storage_dir,
                "educational_attainment.tsv"
            )
            ## gwas_munged is a built-in dataset
            data.table::fwrite(
                x = MAGMA.Celltyping::gwas_munged,
                file = unzipped_path,
                sep = "\t"
            )
        } else {
            path <- get_data(
                fname = paste0(trait, ".ukb.tsv.gz"),
                storage_dir = storage_dir
            )
            unzipped_path <- decompress(path_formatted = path)
        }
    } else {
        unzipped_path <- get_example_gwas_raw(
            storage_dir = storage_dir,
            trait = trait,
            timeout = timeout
        )
    }
    return(unzipped_path)
}
