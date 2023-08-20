check_genomeLocFile <- function(genome_build,
                                path_formatted,
                                storage_dir){ 
    if (toupper(genome_build) %in% c("GRCH36")) {
        genomeLocFile <- get_genomeLocFile(build = "GRCH36",
                                           storage_dir = storage_dir)
    } else if (toupper(genome_build) %in% c("GRCH37","HG37","HG19")) { 
        genomeLocFile <- get_genomeLocFile(build = "GRCH37",
                                           storage_dir = storage_dir)
    } else if (toupper(genome_build) %in% c("GRCH38","HG38")) { 
        genomeLocFile <- get_genomeLocFile(build = "GRCH38",
                                           storage_dir = storage_dir)
    } else {
        stop("Genome build must be: 'GRCH36', `GRCH37', or 'GRCH38'")
    }
    return(genomeLocFile)
}