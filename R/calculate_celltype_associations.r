#' Calculate celltype associations using MAGMA
#'
#' Assumes that you have already run
#' \link[MAGMA.Celltyping]{map_snps_to_genes}.
#'
#' @param ctd Cell type data structure containing $specificity_quantiles.
#' @param gwas_sumstats_path File path of the summary statistics file.
#' @param analysis_name Used in file names which area created.
#' @param ctd_levels Which levels of \code{ctd} to
#' iterate the enrichment analysis over.
#' @param prepare_ctd Whether to run
#' \link[MAGMA.Celltyping]{prepare_quantile_groups} on the \code{ctd} first.
#' @param upstream_kb How many kb upstream of the gene
#'  should SNPs be included?
#' @param downstream_kb How many kb downstream of the gene
#' should SNPs be included?
#' @param genome_ref_path Path to the folder containing the 1000
#' genomes reference (downloaded with \link[MAGMA.Celltyping]{get_genome_ref}).
#' @param sctSpecies Species name relevant to the cell type data.
#' See \link[EWCE]{list_species} for all available species.
#' @param genesOutCOND [Optional] Path to a genes.out file to condition on.
#' Used if you want to condition on a different GWAS.
#' @param EnrichmentMode [Optional] Should either 'Linear' or 'Top 10\%' mode
#'  be used for testing enrichment?
#' @param force_new [Optional] Force new MAGMA analyses even if the
#'  pre-existing results files are detected.
#' @param verbose Print messages.
#'
#' @returns File path for the genes.out file
#'
#' @examples
#' #### Prepare data ####
#' #' ctd <- ewceData::ctd()
#' path_formatted <- MAGMA.Celltyping::get_example_gwas()
#'
#' ##### Map SNPs to genes ####
#' genesOutPath <- MAGMA.Celltyping::map_snps_to_genes(
#'     path_formatted = path_formatted,
#'     genome_build = "GRCh37"
#' )
#'
#' #### Run pipeline ####
#' ctAssocs <- MAGMA.Celltyping::calculate_celltype_associations(
#'     ctd = ctd,
#'     gwas_sumstats_path = path_formatted,
#'     sctSpecies = "mouse"
#' )
#' @export
#' @keywords main_function
calculate_celltype_associations <- function(ctd,
                                            gwas_sumstats_path,
                                            analysis_name = "MainRun",
                                            ctd_levels = seq_len(length(ctd)),
                                            prepare_ctd = TRUE,
                                            upstream_kb = 35,
                                            downstream_kb = 10,
                                            genome_ref_path = NULL,
                                            sctSpecies = "mouse",
                                            genesOutCOND = NA,
                                            EnrichmentMode = "Linear",
                                            force_new = FALSE,
                                            verbose = TRUE) {

    #### Process args ####
    check_enrichment_mode(EnrichmentMode = EnrichmentMode)
    #### prepare quantile groups ####
    # MAGMA.Celltyping can only use human GWAS
    if (prepare_ctd) {
        output_species <- "human"
        ctd <- prepare_quantile_groups(
            ctd = ctd,
            input_species = sctSpecies,
            output_species = output_species,
            verbose = verbose
        )
        sctSpecies <- output_species
    }
    #### Prepare genome_ref ####
    genome_ref_path <- get_genome_ref(
        genome_ref_path = genome_ref_path,
        verbose = verbose
    )
    #### Setup paths ####
    gwas_sumstats_path <- path.expand(gwas_sumstats_path)
    magmaPaths <- get_magma_paths(
        gwas_sumstats_path = gwas_sumstats_path,
        upstream_kb = upstream_kb,
        downstream_kb = downstream_kb
    )
    #### Check for errors in arguments ####
    check_inputs_to_magma_celltype_analysis(
        ctd = ctd,
        gwas_sumstats_path = gwas_sumstats_path,
        analysis_name = analysis_name,
        upstream_kb = upstream_kb,
        downstream_kb = downstream_kb,
        genome_ref_path = genome_ref_path
    )

    output <- list()
    for (annotLevel in ctd_levels) {
        #### Prepare output list ####
        tmp <- list()
        sumstatsPrefix2 <- sprintf(
            "%s.level%s",
            magmaPaths$filePathPrefix, annotLevel
        )
        path <- sprintf("%s.%s.gsa.out", sumstatsPrefix2, analysis_name)
        geneCovarFile <- NULL
        print(path)

        if ((!file.exists(path)) | force_new) {
            messager("Running MAGMA.", v = verbose)

            if (EnrichmentMode == "Linear") {
                # First match quantiles to the genes in the genes.out file...
                # then write as the genesCovar file (the input to MAGMA)
                geneCovarFile <- create_gene_covar_file(
                    genesOutFile = sprintf(
                        "%s.genes.out",
                        magmaPaths$filePathPrefix
                    ),
                    ctd = ctd,
                    annotLevel = annotLevel,
                    sctSpecies = sctSpecies,
                    genesOutCOND = genesOutCOND
                )

                if (is.na(genesOutCOND[1])) {
                    magma_cmd <- sprintf(
                        paste(
                            "magma",
                            "--gene-results '%s.genes.raw'",
                            "--gene-covar '%s'",
                            "--model direction=pos --out '%s.%s'"
                        ),
                        magmaPaths$filePathPrefix,
                        geneCovarFile,
                        sumstatsPrefix2,
                        analysis_name
                    )
                } else {
                    conditionOn <- paste(sprintf(
                        "ZSTAT%s",
                        seq_len(length(genesOutCOND))
                    ),
                    collapse = ","
                    )
                    magma_cmd <- sprintf(
                        paste(
                            "magma",
                            "--gene-results '%s.genes.raw'",
                            "--gene-covar '%s'",
                            "--model direction=pos  condition-residualize='%s'",
                            "--out '%s.%s'"
                        ),
                        magmaPaths$filePathPrefix,
                        geneCovarFile,
                        conditionOn,
                        sumstatsPrefix2,
                        analysis_name
                    )
                }
            } else if (EnrichmentMode == "Top 10%") {
                # First match quantiles to the genes in the genes.out file...
                # then write as the genesCovar file (the input to MAGMA)
                geneCovarFile <- create_top10percent_genesets_file(
                    genesOutFile = sprintf(
                        "%s.genes.out",
                        magmaPaths$filePathPrefix
                    ),
                    ctd = ctd,
                    annotLevel = annotLevel,
                    sctSpecies = sctSpecies
                )

                if (is.na(genesOutCOND[1])) {
                    magma_cmd <- sprintf(
                        paste(
                            "magma",
                            "--gene-results '%s.genes.raw'",
                            "--set-annot '%s'",
                            "--out '%s.%s'"
                        ),
                        magmaPaths$filePathPrefix,
                        geneCovarFile,
                        sumstatsPrefix2,
                        analysis_name
                    )
                } else {
                    geneCovarFile2 <- create_gene_covar_file(
                        genesOutFile = sprintf(
                            "%s.genes.out",
                            magmaPaths$filePathPrefix
                        ),
                        ctd = ctd,
                        annotLevel = annotLevel,
                        sctSpecies = sctSpecies,
                        genesOutCOND = genesOutCOND
                    )
                    conditionOn <- paste(sprintf(
                        "ZSTAT%s",
                        1:length(genesOutCOND)
                    ),
                    collapse = ","
                    )
                    magma_cmd <- sprintf(
                        paste(
                            "magma",
                            "--gene-results '%s.genes.raw'",
                            "--set-annot '%s' twosided",
                            "--gene-covar '%s' condition-only='%s'",
                            "--out '%s.%s'"
                        ),
                        magmaPaths$filePathPrefix,
                        geneCovarFile,
                        geneCovarFile2,
                        conditionOn,
                        sumstatsPrefix2,
                        analysis_name
                    )
                }
            }
            print(magma_cmd)
            system(magma_cmd)
        } else {
            messager("Importing precomputed MAGMA results.", v = verbose)
        }
        tmp$geneCovarFile <- geneCovarFile
        tmp$results <- load_magma_results_file(
            path = path,
            annotLevel = annotLevel,
            ctd = ctd,
            genesOutCOND = genesOutCOND,
            EnrichmentMode = EnrichmentMode,
            ControlForCT = "BASELINE"
        )
        output[[length(output) + 1]] <- tmp
    } # //End for loop

    # Calculate total number of tests performed
    totalTests <- 0
    for (annotLevel in 1:length(output)) {
        totalTests <- totalTests + dim(output[[annotLevel]]$results)[1]
    }
    output$total_baseline_tests_performed <- totalTests

    output$gwas_sumstats_path <- gwas_sumstats_path
    output$analysis_name <- analysis_name
    output$upstream_kb <- upstream_kb
    output$downstream_kb <- downstream_kb
    output$genome_ref_path <- genome_ref_path

    return(output)
}
