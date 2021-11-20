#' Map SNPs to their nearby genes
#'
#' Make two external calls to MAGMA. First use it to annotate SNPs
#'  onto their neighbouring genes. Second, use it to calculate
#'  the gene level trait association.
#'
#' @param path_formatted Filepath of the summary statistics file
#' (which is expected to already be in the required format).
#' Can be uncompressed or compressed (".gz" or ".bgz").
#' @param genome_build The build of the reference genome
#'  (\code{"GRCh37"} or \code{"GRCh38"}).
#' If \code{NULL}, it will be inferred with
#' \link[MungeSumstats]{get_genome_build}.
#' @param upstream_kb How many kilobases (kb) upstream of the gene
#'  should SNPs be included?
#' @param downstream_kb How many kilobases (kb) downstream of the gene
#' should SNPs be included?
#' @param N What is the N number for this GWAS? That is cases + controls.
#' @param genome_ref_path Path to the folder containing the
#' 1000 genomes .bed files (which can be downloaded from 
#' \href{https://ctg.cncr.nl/software/MAGMA/ref_data/g1000_eur.zip}{here}).
#' @param force_new Set to \code{TRUE} to
#' rerun \code{MAGMA} even if the output files already exist.
#'  (Default: \code{FALSE}).
#' @inheritParams get_genome_ref
#'
#' @return Path to the genes.out file.
#'
#' @examples
#' path_formatted <- MAGMA.Celltyping::get_example_gwas()
#' genesOutPath <- MAGMA.Celltyping::map_snps_to_genes(
#'     path_formatted = path_formatted,
#'     genome_build = "hg19",
#'     N = 5000)
#' @export
map_snps_to_genes <- function(path_formatted,
                              genome_build = NULL,
                              upstream_kb = 35,
                              downstream_kb = 10,
                              N = NULL,
                              genome_ref_path = NULL,
                              population = "eur",
                              storage_dir = tempdir(),
                              force_new = FALSE,
                              verbose = TRUE) {
    genome_ref_path <- get_genome_ref(genome_ref_path = genome_ref_path,
                                      storage_dir = storage_dir,
                                      population = population,
                                      verbose = verbose)
    path_formatted <- path.expand(path_formatted)
    magmaPaths <- get_magma_paths(
        gwas_sumstats_path = path_formatted,
        upstream_kb = upstream_kb,
        downstream_kb = downstream_kb
    )
    # Remove a trailing slash to avoid errors on windows
    outPath <- gsub("\\/$", "", magmaPaths$filePathPrefix)
    genes_annot <- sprintf("%s.genes.annot", outPath)
    genes_out <- sprintf("%s.genes.out", outPath)

    if ((file.exists(genes_annot) &
        file.exists(genes_out)) &
        (force_new == FALSE)) {
        message("Precomputed file detected: ", genes_out)
        return(genes_out)
    }
    dir.create(dirname(genes_out), showWarnings = FALSE, recursive = TRUE)
    #### MAGMA requires files to be decompressed #####
    path_formatted <- decompress(path_formatted = path_formatted)
    # Check whether there is an N column in the sumstats file
    # (if it wasn't provided as an argument)
    if (is.null(N)) {
        first_line <- readLines(path_formatted, n = 1)
        column_headers <- strsplit(first_line, "\t")[[1]]
        if ("N" %in% column_headers) {
            n_arg <- "ncol=N"
        } else {
            nval <- as.numeric(
                readline(paste(
                    "There is no N column within the sumstats file.",
                    "What is the N value for this GWAS?"
                ))
            )

            if (is.na(nval)) {
                stop(paste(
                    nval, "provided but value of N for",
                    "the GWAS must be numeric"
                ))
            }
            if (nval < 1000) {
                stop(paste(
                    "Value of N provided is less than 1,000.",
                    "This seems unlikely."
                ))
            }
            if (nval > 100000000) {
                stop(paste(
                    "Value of N provided is over than 100,000,000.",
                    "This seems unlikely."
                ))
            }
            n_arg <- sprintf("N=%s", nval)
        }
    } else {
        n_arg <- sprintf("N=%s", N)
    }

    # Determine which genome build it uses & get path to gene loc file
    gene_loc_dir <- system.file("data",package = "MAGMA.Celltyping")
    if (is.null(genome_build)) {
        genome_build <-
            MungeSumstats::get_genome_builds(sumstats_list = path_formatted,
                                             names_from_paths = TRUE)
    }
    if (toupper(genome_build) %in% c("GRCH37","HG37","HG19")) {
        genomeLocFile <- sprintf("%s/NCBI37.3.gene.loc", gene_loc_dir)
    } else if (toupper(genome_build) %in% c("GRCH38","HG38")) {
        genomeLocFile <- sprintf("%s/NCBI38.gene.loc", gene_loc_dir)
    } else {
        stop("Genome build must be: 'GRCH37', or 'GRCH38'")
    }
    #### Create genes.annot ####
    message_parallel("\n==== MAGMA Step 1: Generate genes.annot file ====\n")
    magma_cmd <- sprintf(
        paste("magma",
              "--annotate window=%s,%s",
              "--snp-loc '%s'",
              "--gene-loc '%s'",
              "--out '%s'"),
        upstream_kb, 
        downstream_kb, 
        path_formatted, 
        genomeLocFile, 
        outPath
    )
    system(magma_cmd)

    #### Create genes.out ####
    message_parallel("\n==== MAGMA Step 2: Generate genes.out ====\n")
    magma_cmd <- sprintf(
        paste("magma",
              "--bfile '%s'",
              "--pval '%s' %s",
              "--gene-annot '%s.genes.annot'",
              "--out '%s'"),
        path.expand(genome_ref_path), path_formatted,
        n_arg, magmaPaths$filePathPrefix, outPath
    )
    # magma_cmd
    system(magma_cmd)
    # Return path to genes.out file
    return(genes_out)
}

map.snps.to.genes <- function(...){
    .Deprecated("map_snps_to_genes")
    map_snps_to_genes(...)
}
