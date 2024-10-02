#' Download genome ref
#'
#' Download and decompress the genome reference needed to run
#' \link[MAGMA.Celltyping]{map_snps_to_genes},
#' If the file already exists, it will simply return the path.
#'
#' @param genome_ref_path If not \code{NULL} and file exists,
#'  this file path will be returned.
#' @param storage_dir Where to store genome ref.
#' @param method Get reference genome data from the
#' \href{https://ctg.cncr.nl/software/MAGMA/ref_data/}{MAGMA server} (slow).
#' @param population Which population subset of the genome reference
#'  to include.
#'  \itemize{
#'  \item{"eur" : }{European descent
#'  (Default simply because this is currently
#'  the most common GWAS subpopulation).}
#'  \item{"afr" : }{African descent.}
#'  \item{"amr" : }{Ad Mixed American descent.}
#'  \item{"eas" : }{East Asian descent.}
#'  \item{"sas" : }{South Asian descent.}
#'  }
#' @param timeout Number seconds to wait before ending the download
#'  (Default: 5 minutes).
#' @param verbose Print messages.
#'
#' @return Directory where the genome reference data is stored.
#'
#' @export
#' @importFrom utils download.file unzip
#' @importFrom tools R_user_dir
#' 
#' @examples
#' \dontrun{
#' genome_ref_path <- get_genome_ref()
#' }
get_genome_ref <- function(genome_ref_path = NULL,
                           storage_dir = tools::R_user_dir(
                               "MAGMA.Celltyping",
                               which="cache"),
                           method = c("magma"),
                           population = c("eur", "afr", "amr", "eas", "sas"),
                           timeout = 60 * 5,
                           verbose = TRUE) {
    #### population ####
    dir.create(storage_dir,showWarnings = FALSE, recursive = TRUE)
    population <- tolower(population[1])
    population <- match.arg(population)
    method <- tolower(method[1])
    #### Set up paths ####
    if (is.null(genome_ref_path)) {
        genome_ref_dir <- file.path(storage_dir, 
                                    paste0("g1000_", population))
        genome_ref_path <- file.path(genome_ref_dir,
                                     paste0("g1000_", population))
    } else {
        genome_ref_dir <- dirname(genome_ref_path)
    }
    genome_ref_path <- fix_path(genome_ref_path)
    #### Check if the genome ref is already available ####
    if (file.exists(paste0(genome_ref_path, ".bed"))) {
        messager("Using existing genome_ref found in storage_dir.",
            v = verbose
        )
    } else {
        #### Download anew ####
        messager("genome_ref not found in storage_dir.",
            "Downloading from remote server instead ==>",
            genome_ref_dir,
            v = verbose
        )
        if(method == "magma"){
            get_genome_ref_magma(
                genome_ref_dir = genome_ref_dir, 
                timeout = timeout,
                verbose = verbose
            )
        }
      ### Reference file is too large to be stored in GitHub Releases now
      #else if(method == "piggyback"){
            # get_genome_ref_piggyback(
            #     genome_ref_dir = genome_ref_dir, 
            #     verbose = verbose
            # )
    }
    #### Return the folder + the file prefix ####
    return(genome_ref_path)
}
