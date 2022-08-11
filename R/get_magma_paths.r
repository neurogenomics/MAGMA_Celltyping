#' Get MAGMA Paths
#'
#' Gets paths to folders based on location of the GWAS summary statistics file. 
#' This avoids the GWAS sumstats folder becoming a mess.
#'
#' @param gwas_sumstats_path Path to the GWAS summary statistics file.
#' @param upstream_kb How many kb upstream of the gene 
#' should SNPs be included?
#' @param downstream_kb How many kb downstream of the gene 
#' should SNPs be included?
#' @param output_path If being called from 
#' \link[MAGMA.Celltyping]{magma_tileplot} 
#' then \code{gwas_sumstats_path} cannot be specified, 
#' so instead specificity the base folder where MAGMA_Figures exists.
#'
#' @return File path for where MAGMA files should be created
#'
#' @source 
#' \code{
#' gwas_sumstats_path <- MAGMA.Celltyping::get_example_gwas(
#'     trait = "educational_attainment")
#' magmaPaths <- MAGMA.Celltyping:::get_magma_paths(
#'     gwas_sumstats_path = gwas_sumstats_path, 
#'     upstream_kb = 35, 
#'     downstream_kb = 10)
#' }
#' @export
get_magma_paths <- function(gwas_sumstats_path = NA,
                            upstream_kb = NA,
                            downstream_kb = NA,
                            output_path = NA) {
    # Error checking
    if (is.na(gwas_sumstats_path) &
        is.na(output_path)) {
        stop("Either gwas_sumstats_path or output_path must be specified")
    }
    if (!is.na(gwas_sumstats_path) &
        (is.na(upstream_kb) |
            is.na(downstream_kb))) {
        stop(paste(
            "If gwas_sumstats_path is specified then upstream_kb",
            "and downstream_kb must also be specified"
        ))
    }
    # If output_path is not specified, 
    # then use the folder containing gwas_sumstats
    if (is.na(output_path)) {
        output_path <- dirname(gwas_sumstats_path)
    }
    output_path <- fix_path(output_path)
    dir.create(output_path, showWarnings = FALSE, recursive = TRUE) 

    if (!is.na(gwas_sumstats_path)) {
        gwas_sumstats_path <- fix_path(gwas_sumstats_path)
        gwasFileName <- basename(gwas_sumstats_path)
        # Set the paths
        pathMagmaFiles <- sprintf(
            "%s/MAGMA_Files/%s.%sUP.%sDOWN",
            output_path, gwasFileName, upstream_kb, downstream_kb
        )
        pathFigs <- sprintf(
            "%s/MAGMA_Figures/%s/FIGS",
            output_path, gwasFileName
        )
        pathTiles <- sprintf(
            "%s/MAGMA_Figures/Tileplots",
            output_path
        )
        # Normalize paths  
        pathMagmaFiles <- fix_path(pathMagmaFiles)
        pathTiles <- fix_path(pathTiles)
        pathFigs <- fix_path(pathFigs)  
        # Create the folders (in case they don't exist yet) 
        dir.create(pathMagmaFiles, showWarnings = FALSE, recursive = TRUE) 
        dir.create(pathTiles, showWarnings = FALSE, recursive = TRUE)
        dir.create(pathFigs, showWarnings = FALSE, recursive = TRUE)

        prefix <- fix_path(
            sprintf("%s.%sUP.%sDOWN",
                    gwasFileName, 
                    upstream_kb, 
                    downstream_kb)
        )
        filePathPrefix <- sprintf("%s/%s.%sUP.%sDOWN",
                                  pathMagmaFiles, gwasFileName,
                                  upstream_kb, downstream_kb)
        filePathPrefix <- fix_path(filePathPrefix)

        # Return the paths
        magmaPaths <- list(tiles = pathTiles,
                           figs = pathFigs, 
                           files = pathMagmaFiles,
                           prefix = prefix, 
                           filePathPrefix = filePathPrefix, 
                           gwasFileName = gwasFileName, 
                           gwasFilePath = gwas_sumstats_path)
    } else { 
        pathTiles <- fix_path(
            sprintf("%s/MAGMA_Figures/Tileplots", output_path)
        )
        pathFigs <- fix_path(
            sprintf("%s/MAGMA_Figures", output_path)
        )   
        dir.create(pathTiles, showWarnings = FALSE, recursive = TRUE)
        dir.create(pathFigs, showWarnings = FALSE, recursive = TRUE)
        magmaPaths <- list(tiles = pathTiles,
                           figs = pathFigs)
    } 
    return(magmaPaths)
}

get.magma.paths <- function(...){
    .Deprecated("get_magma_paths")
    get_magma_paths(...)
}
