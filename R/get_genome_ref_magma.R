get_genome_ref_magma <- function(genome_ref_dir,
                                 input_url = file.path(
                                     "https://ctg.cncr.nl/software/MAGMA",
                                     "ref_data/g1000_eur.zip"),
                                 verbose = TRUE) {
    ##### Link to 1000 genomes reference data. ####
    zipfile <- sprintf("%s.zip", genome_ref_dir)
    #### Download file ####
    options(timeout = 60 * 5)
    utils::download.file(
        url = input_url,
        destfile = zipfile
    )
    #### Unzip file ####
    messager("Unzipping file.", v = verbose) 
    utils::unzip(
        zipfile = zipfile,
        exdir = genome_ref_dir
    )
   # try({
   #     if(file.exists(zipfile)){
   #         out <- file.remove(zipfile, 
   #                            overwrite = TRUE,
   #                            showWarnings = FALSE)
   #     }
   # })
}
