#' Download genome ref: magma
#'
#' Download and decompress the genome reference needed to run
#' \link[MAGMA.Celltyping]{map_snps_to_genes},
#' If the file already exists, it will simply return the path.
#' 
#' @param genome_ref_dir Folder to download reference to.
#' @param verbose Print messages.
#' @source \href{https://cncr.nl/research/magma/}{MAGMA site} 
#' 
#' @keywords internal
get_genome_ref_magma <- function(genome_ref_dir, 
                                 population = "eur",
                                 timeout = 60 * 5,
                                 verbose = TRUE) {
    #### Get link ####
    link_dict <- list(
      "g1000_eur"="https://vu.data.surfsara.nl/index.php/s/VZNByNwpD8qqINe/download",
      "g1000_afr"="https://vu.data.surfsara.nl/index.php/s/ePXET6IWVTwTes4/download",
      "g1000_eas"="https://vu.data.surfsara.nl/index.php/s/dz6PYdKOi3xVqHn/download",
      "g1000_sas"="https://vu.data.surfsara.nl/index.php/s/C6UkTV5nuFo8cJC/download",
      "g1000_amr"="https://vu.data.surfsara.nl/index.php/s/TXDEm70eEO7AgOb/download"
    )
    input_url <- link_dict[[paste0("g1000_",tolower(population))]]
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
