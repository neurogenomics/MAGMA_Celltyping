#' Calculate celltype associations using MAGMA
#'
#' Assumes that you have already run
#' \link[MAGMA.Celltyping]{map_snps_to_genes}.
#'
#' @param gwas_sumstats_path File path of the summary statistics file.
#' @param magma_dir Path to folder containing the
#' pre-computed MAGMA GWAS files (\emph{.gsa.raw}and \emph{.gsa.out}).
#' @param analysis_name Used in file names which area created.
#' @param prepare_ctd Whether to run
#' \link[MAGMA.Celltyping]{prepare_quantile_groups} on the \code{ctd} first.
#' @param genesOutCOND [Optional] Path to a genes.out file to condition on.
#' Used if you want to condition on a different GWAS.
#' @param EnrichmentMode [Optional] Should either 'Linear' or 'Top 10\%' mode
#'  be used for testing enrichment?
#' @inheritParams celltype_associations_pipeline
#'
#' @returns File path for the genes.out file
#'
#' @export
#' @importFrom stats setNames
#' @keywords main_function
#' @examples
#' #### Prepare cell-type data ####
#' ctd <- ewceData::ctd()
#' 
#' #### Prepare GWAS MAGMA data ####
#' magma_dir <- MAGMA.Celltyping::import_magma_files(ids = "ieu-a-298")
#'     
#' #### Run pipeline ####
#' ctAssocs <- calculate_celltype_associations(
#'     ctd = ctd,
#'     ctd_levels = 1,
#'     magma_dir = magma_dir,
#'     ctd_species = "mouse")
calculate_celltype_associations <- function(ctd,
                                            ctd_levels = seq_len(length(ctd)),
                                            ctd_species = infer_ctd_species(ctd),
                                            gwas_sumstats_path = NULL,
                                            magma_dir = NULL,
                                            analysis_name = "MainRun", 
                                            prepare_ctd = TRUE,
                                            upstream_kb = 35,
                                            downstream_kb = 10,
                                            genesOutCOND = NA,
                                            EnrichmentMode = "Linear",
                                            force_new = FALSE,
                                            version = NULL,
                                            verbose = TRUE) {
    # devoptera::args2vars(calculate_celltype_associations)
    
    #### Check MAGMA installation ####
    magma_version <- magma_check(version = version,
                                 return_version = TRUE,
                                 verbose = verbose)
    
    #### Process args ####
    analysis_name <- check_analysis_name(EnrichmentMode = EnrichmentMode,
                                         analysis_name = analysis_name)
    check_enrichment_mode(EnrichmentMode = EnrichmentMode)
    #### Handle MAGMA Files ####
    #### Trick downstream functions into working with only MAGMA files ####
    magma_dir <- magma_dir[1]
    if(!is.null(magma_dir)){ 
        gwas_sumstats_path <- create_fake_gwas_path(
            magma_dir = magma_dir,
            upstream_kb = upstream_kb,
            downstream_kb = downstream_kb)
    }
    gwas_sumstats_path <- fix_path(gwas_sumstats_path)
    #### prepare quantile groups ####
    # MAGMA.Celltyping can only use human GWAS
    if (isTRUE(prepare_ctd)) {
        output_species <- "human"
        ctd <- prepare_quantile_groups(
            ctd = ctd,
            input_species = ctd_species,
            output_species = output_species,
            verbose = verbose
        )
        ctd_species <- output_species
    } 
    #### Setup paths ####
    magmaPaths <- get_magma_paths(
        gwas_sumstats_path = gwas_sumstats_path,
        upstream_kb = upstream_kb,
        downstream_kb = downstream_kb
    )
    #### Check for errors in arguments ####
    check_inputs_to_magma_celltype_analysis(
        ctd = ctd,
        gwas_sumstats_path = gwas_sumstats_path, 
        upstream_kb = upstream_kb,
        downstream_kb = downstream_kb
    )
    #### Iterate over each CTD level ####
    output <- lapply(stats::setNames(ctd_levels,
                                     paste0("level",ctd_levels)),
                     function(annotLevel){
        sumstatsPrefix2 <- sprintf(
            "%s.level%s",
            magmaPaths$filePathPrefix, annotLevel
        )
        path <- sprintf("%s.%s.gsa.out",
                        sumstatsPrefix2,
                        analysis_name)
        path <- get_actual_path(path)
        #### Use existing results, or compute new ones ####
        if ((!file.exists(path)) || isTRUE(force_new)) {
            messager("Running MAGMA:",EnrichmentMode,"mode", v = verbose) 
            #### Linear mode ####
            if (EnrichmentMode == "Linear") {
                cca_out <- calculate_celltype_associations_linear(
                    magmaPaths=magmaPaths,
                    ctd=ctd,
                    annotLevel=annotLevel,
                    ctd_species=ctd_species,
                    genesOutCOND=genesOutCOND,
                    sumstatsPrefix2=sumstatsPrefix2,
                    analysis_name=analysis_name)
            #### Top 10% mode ####
            } else if (EnrichmentMode == "Top 10%") {
                cca_out <- calculate_celltype_associations_top10(
                    magmaPaths=magmaPaths,
                    ctd=ctd,
                    annotLevel=annotLevel,
                    ctd_species=ctd_species,
                    genesOutCOND=genesOutCOND,
                    sumstatsPrefix2=sumstatsPrefix2,
                    analysis_name=analysis_name,
                    magma_version=magma_version)
            }
            magma_cmd <- cca_out$magma_cmd
            geneCovarFile <- cca_out$geneCovarFile
            #### Run MAGMA command ####
            magma_run(cmd = magma_cmd, 
                      version = version)
        } else {
            messager("Importing precomputed MAGMA results.",
                     "magma_cmd and geneCovarFile will be set to NULL.",
                     v = verbose)
            magma_cmd <- NULL
            geneCovarFile <- NULL
        }
        #### Prepare output list ####
        tmp <- list()
        tmp$results <- load_magma_results_file(
            path = path,
            annotLevel = annotLevel,
            ctd = ctd,
            genesOutCOND = genesOutCOND,
            EnrichmentMode = EnrichmentMode,
            analysis_name = analysis_name,
            ControlForCT = "BASELINE", 
            verbose = verbose
        )  
        tmp$results_path <- path
        tmp$magma_cmd <- magma_cmd
        tmp$annotLevel <- annotLevel
        tmp$geneCovarFile <- geneCovarFile
        tmp$genesOutCOND <- genesOutCOND
        tmp$EnrichmentMode <- EnrichmentMode
        return(tmp)
    }) # //End lapply loop

    #### Calculate total number of tests performed ####
    totalTests <- 0
    for (annotLevel in ctd_levels) {
        totalTests <- totalTests + dim(output[[annotLevel]]$results)[1]
    }
    output$total_baseline_tests_performed <- totalTests
    output$gwas_sumstats_path <- gwas_sumstats_path
    output$analysis_name <- analysis_name
    output$upstream_kb <- upstream_kb
    output$downstream_kb <- downstream_kb 
    output$magma_version <- magma_version
    return(output)
}
