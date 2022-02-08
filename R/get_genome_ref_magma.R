#' Download genome ref: magma
#'
#' Download and decompress the genome reference needed to run
#' \link[MAGMA.Celltyping]{map_snps_to_genes},
#' If the file already exists, it will simply return the path.
#' 
#' @param genome_ref_dir Folder to download reference to.
#' @param verbose Print messages.
#' @source \href{https://ctg.cncr.nl/software/MAGMA/ref_data/}{MAGMA archives} 
#' 
#' @keywords internal
get_genome_ref_magma <- function(genome_ref_dir, 
                                 timeout = 60 * 5,
                                 verbose = TRUE) {
    #### Get link ####
    input_url = file.path(
        "https://ctg.cncr.nl/software/MAGMA",
        "ref_data",paste0(basename(genome_ref_dir),".zip"))
    ##### Link to 1000 genomes reference data. ####
    zipfile <- sprintf("%s.zip", genome_ref_dir)
    #### Download file ####
    options(timeout = timeout)
    utils::download.file(
        url = input_url,
        destfile = zipfile
    )
    #### Unzip file ####
    messager("Unzipping file.", v = verbose) 
    utils::unzip(
        zipfile = zipfile,
        exdir = genome_ref_dir,
        overwrite = TRUE
    )
   try({
       if(file.exists(zipfile)){
           out <- file.remove(zipfile,
                              showWarnings = FALSE)
       }
   })
}
