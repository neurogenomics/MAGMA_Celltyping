#' Download genome ref
#'
#' Download and decompress the genome reference needed to run
#' \link[MAGMA.Celltyping]{map_snps_to_genes},
#' If the file already exists, it will simply return the path.
#'
#' @param storage_dir Where to store genome ref.
#' @param method Get reference genome data from the 
#' \href{https://ctg.cncr.nl/software/MAGMA/ref_data}{MAGMA server} (slow) or
#'  the Bioconductor package XXXXX.
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
#' @param verbose Print messages.
#'
#' @return Directory where the genome reference data is stored.
#'
#' @export
#' @importFrom utils download.file unzip
#' @examples
#' \dontrun{
#' genome_ref_path <- get_genome_ref()
#' }
get_genome_ref <- function(storage_dir = tempdir(),
                           method = c("bioc","magma"),
                           population = c("eur","afr","amr","eas","sas"),
                           verbose = TRUE) {
    #### population ####
    population <- tolower(population[1])
    pop_opts <- c("eur","afr","amr","eas","sas")
    if(!population %in% pop_opts){
        stop_msg <- paste0("population must be one of:\n",
                          paste0(" - ",pop_opts,collapse = "\n"))
        stop(stop_msg)
    }
    #### Set up paths ###
    genome_ref_dir <- file.path(storage_dir, paste0("g1000_",population))
    genome_ref_path <- file.path(genome_ref_dir, paste0("g1000_",population))
    #### Check if the genome ref is already available ####
    if (file.exists(paste0(genome_ref_path,".bed"))) {
        messager("Using existing genome_ref found in storage_dir.",
            v = verbose
        )
    } else {
        #### Download anew ####
        messager("genome_ref not found in storage_dir.\n",
            "Downloading from remote server instead ==>",
            genome_ref_dir,
            v = verbose
        )
        get_genome_ref_magma(genome_ref_dir = genome_ref_dir,
                             verbose = verbose)
    }
    #### Return the folder + the file prefix ####
    return(genome_ref_path)
}
