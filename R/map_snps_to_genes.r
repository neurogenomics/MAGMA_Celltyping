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
#' @param genome_ref_path Path to the folder containing the 1000
#' genomes reference (downloaded with \link[MAGMA.Celltyping]{get_genome_ref}).
#' @param genes_only The \emph{.genes.raw} file is the intermediary file 
#' that serves as  the input for subsequent gene-level analyses. 
#'  To perform only a gene analysis, with no subsequent gene-set analysis,
#'  the \code{--genes-only} flag can be added (\code{TRUE}). 
#'  This suppresses the creation of the \emph{.genes.raw} file, 
#'  and significantly reduces the running time and memory required.
#' @param force_new Set to \code{TRUE} to
#' rerun \code{MAGMA} even if the output files already exist.
#'  (Default: \code{FALSE}).
#' @inheritParams get_genome_ref
#' @inheritParams celltype_associations_pipeline
#'
#' @returns Path to the genes.out file.
#' 
#' @export
#' @importFrom MungeSumstats get_genome_builds
#' @importFrom tools R_user_dir
#' @examples
#' \dontrun{
#' path_formatted <- MAGMA.Celltyping::get_example_gwas()
#' genesOutPath <- MAGMA.Celltyping::map_snps_to_genes(
#'     path_formatted = path_formatted,
#'     genome_build = "hg19",
#'     N = 5000)
#' } 
map_snps_to_genes <- function(path_formatted,
                              genome_build = NULL,
                              upstream_kb = 35,
                              downstream_kb = 10,
                              N = NULL,
                              genome_ref_path = NULL,
                              population = "eur",
                              genes_only = FALSE,
                              storage_dir = tools::R_user_dir(
                                  "MAGMA.Celltyping",
                                  which="cache"), 
                              force_new = FALSE,
                              version = NULL,
                              verbose = TRUE) {
    #### Check MAGMA installation ####
    magma_check(version = version, 
                verbose = verbose)
    #### Download  1000 Genomes reference panel ####
    genome_ref_path <- get_genome_ref(genome_ref_path = genome_ref_path,
                                      storage_dir = storage_dir,
                                      population = population,
                                      verbose = verbose)
    path_formatted <- fix_path(path_formatted)
    magmaPaths <- get_magma_paths(
        gwas_sumstats_path = path_formatted,
        upstream_kb = upstream_kb,
        downstream_kb = downstream_kb
    )
    #### Create MAGMA output file paths ####
    outPath <- fix_path(magmaPaths$filePathPrefix)
    genes_annot <- paste0(outPath,".genes.annot")
    genes_out <- paste0(outPath,".genes.out")
    #### Check for existing files ####
    if ((file.exists(genes_annot) &
        file.exists(genes_out)) &
        (force_new == FALSE)) {
        message("Precomputed file detected: ", genes_out)
        return(genes_out)
    }
    dir.create(dirname(genes_out), showWarnings = FALSE, recursive = TRUE)
    #### MAGMA requires files to be decompressed #####
    path_formatted <- decompress(path_formatted = path_formatted, 
                                 storage_dir = tempdir(),
                                 verbose = verbose)
    # Check whether there is an N column in the sumstats file
    # (if it wasn't provided as an argument)
    n_arg <- check_n(path_formatted = path_formatted, 
                     N = N) 
    #### Get genome build ####
    if (is.null(genome_build)) {
        genome_build <-
            MungeSumstats::get_genome_builds(sumstats_list = path_formatted,
                                             names_from_paths = TRUE)
    }
    #### Determine which genome build it uses & get path to gene loc file ####
    genomeLocFile <- check_genomeLocFile(genome_build = genome_build,
                                         path_formatted = path_formatted)
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
    #### Run MAGMA command ####
    magma_run(cmd = magma_cmd, 
              version = version) 
    #### Create genes.out ####
    message_parallel("\n==== MAGMA Step 2: Generate genes.out ====\n")
    magma_cmd <- sprintf(
        paste("magma",
              "--bfile '%s'",
              "--pval '%s' %s",
              if(isTRUE(genes_only)) "--genes-only" else NULL,
              "--gene-annot '%s.genes.annot'",
              "--out '%s'"),
        genome_ref_path, 
        path_formatted,n_arg,
        magmaPaths$filePathPrefix,
        outPath
    )
    #### Run MAGMA command ####
    magma_run(cmd = magma_cmd, 
              version = version)
    # Return path to genes.out file
    return(genes_out)
}

map.snps.to.genes <- function(...){
    .Deprecated("map_snps_to_genes")
    map_snps_to_genes(...)
}
