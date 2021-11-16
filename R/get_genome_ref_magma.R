get_genome_ref_magma <- function(genome_ref_dir,
                                 input_url = file.path(
                                     "https://ctg.cncr.nl/software/MAGMA",
                                     "ref_data/g1000_eur.zip"),
                                 verbose = TRUE) {
    ##### Link to 1000 genomes reference data. ####
    destfile <- sprintf("%s.zip", genome_ref_dir)
    #### Download file ####
    options(timeout = 60 * 5)
    utils::download.file(
        url = input_url,
        destfile = destfile
    )
    #### Unzip file ####
    messager("Unzipping file.", v = verbose)
    utils::unzip(
        zipfile = sprintf("%s.zip", genome_ref_dir),
        exdir = genome_ref_dir
    )
    out <- file.remove(sprintf("%s.zip", genome_ref_dir), 
                       showWarnings = FALSE)
}
