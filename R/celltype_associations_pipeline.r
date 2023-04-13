#' Calculate cell type associations using MAGMA
#'
#' Has the option of running multiple analyses with a single function.
#' Assumes that you already have all MAGMA GWAS files precomputed.
#' Precomputed MAGMA GWAS files can be downloaded via the
#' \link[MAGMA.Celltyping]{import_magma_files} function.
#'
#' @param ctd Cell type data structure containing
#'  \code{specificity_quantiles}.
#' @param ctd_species Species name relevant to the CellTypeDataset (\code{ctd}).
#' See \link[EWCE]{list_species} for all available species. 
#' If \code{ctd_species=NULL} (default),
#'  the \code{ctd} species will automatically 
#'  be inferred using \link[orthogene]{infer_species}. 
#' @param ctd_levels Which levels of \code{ctd} to
#' iterate the enrichment analysis over.
#' @param ctd_name Used in file names
#' @param magma_dirs Path to folders containing the
#' pre-computed MAGMA GWAS files (\emph{.gsa.raw}and \emph{.gsa.out}).
#' \emph{NOTE}: Files within these folders must have the same naming scheme
#' as the folders themselves.
#' @param upstream_kb How many kb upstream of the gene
#'  should SNPs be included?
#' @param downstream_kb How many kb downstream of the gene
#' should SNPs be included?
#' @param run_linear Run in linear mode.
#' @param run_top10 Run  in top 10\% mode.
#' @param run_conditional Run in conditional mode.
#' @param suffix_linear This will be added to the linear results file name.
#' @param suffix_top10 This will be added to the top 10\% results file name.
#' @param suffix_condition This will be added to the
#'  conditional results file name.
#' @param save_dir Folder to save results in (\code{save_dir=NULL}
#'  to not save any results).
#' @param force_new [Optional] Force new MAGMA analyses even if the
#'  pre-existing results files are detected.
#' @param nThread Number of threads to parallelise analyses across.
#' @param version MAGMA version to use.
#' @param verbose Print messages.
#' @inheritParams calculate_celltype_associations
#' @inheritParams calculate_conditional_celltype_associations
#' @inheritParams prepare_quantile_groups
#' @returns A list containing the results of each selected
#' celltype associations analysis.
#' 
#' @keywords main_function
#' @export
#' @importFrom parallel mclapply
#' @examples
#' magma_dirs <- MAGMA.Celltyping::import_magma_files(ids = c("ieu-a-298"))
#' ctd <- ewceData::ctd()
#'
#' res <- MAGMA.Celltyping::celltype_associations_pipeline(
#'     ctd = ctd,
#'     ctd_levels = 1,
#'     ctd_name = "Zeisel2015",
#'     ctd_species = "mouse",
#'     magma_dirs = magma_dirs)
celltype_associations_pipeline <- function(ctd,
                                           ctd_levels = seq_len(length(ctd)),
                                           ctd_name,
                                           ctd_species = infer_ctd_species(ctd),
                                           standardise = TRUE,
                                           magma_dirs, 
                                           run_linear = TRUE,
                                           run_top10 = TRUE,
                                           run_conditional = FALSE,
                                           upstream_kb = 35,
                                           downstream_kb = 10,
                                           suffix_linear = "linear",
                                           suffix_top10 = "top10",
                                           suffix_condition = "condition",
                                           controlledAnnotLevel = 1,
                                           controlTopNcells = 1,
                                           force_new = FALSE,
                                           save_dir = tempdir(),
                                           nThread = 1,
                                           version = NULL,
                                           verbose = TRUE) { 
    #### Establish vars in case some are not computed ####
    ctAssocsLinear <- NULL
    ctAssocsTop <- NULL
    ctCondAssocs <- NULL
    ctAssocMerged <- NULL
    #### Check required inputs ####
    force(ctd)
    force(ctd_name)
    force(magma_dirs) 
    #### prepare quantile groups ####
    # MAGMA.Celltyping can only use human GWAS
    {
        messager("Preparing CellTypeDataset.",v=verbose)
        output_species <- "human"
        ctd <- prepare_quantile_groups(
            ctd = ctd,
            input_species = ctd_species,
            output_species = output_species,
            standardise = standardise,
            verbose = verbose>1
        )
        ctd_species <- output_species
    } 
    #### Iterate over GWAS datasets ####
    MAGMA_results <- parallel::mclapply(magma_dirs, function(magma_dir) {
        messager(basename(magma_dir),
                 v = verbose,
                 parallel = nThread>1)
        #### Trick downstream functions into working with only MAGMA files ####
        fake_gwas_ss <- create_fake_gwas_path(magma_dir = magma_dir,
                                              upstream_kb = upstream_kb,
                                              downstream_kb = downstream_kb)
        #### Linear mode ####
        if (isTRUE(run_linear)) {
            messager("=======",
                     "Calculating celltype associations: linear mode",
                     "=======",
                v = verbose
            )
            ctAssocsLinear <- tryCatch(expr = {
                calculate_celltype_associations(
                    EnrichmentMode = "Linear",
                    ctd = ctd,
                    ctd_levels = ctd_levels,
                    prepare_ctd = FALSE, # Already prepared once above
                    gwas_sumstats_path = fake_gwas_ss, 
                    upstream_kb = upstream_kb,
                    downstream_kb = downstream_kb,
                    analysis_name = paste(ctd_name, suffix_linear, sep = "_"),
                    ctd_species = ctd_species,
                    force_new = force_new,
                    version = version,
                    verbose = verbose
                )
            }, error = function(e) {messager(e,v=verbose);NULL}
            )
        } 
        #### Top 10% mode ####
        if (isTRUE(run_top10)) {
            messager("=======",
                     "Calculating celltype associations: top10% mode",
                     "=======",
                v = verbose
            )
            ctAssocsTop <- tryCatch(expr = {
                calculate_celltype_associations(
                    EnrichmentMode = "Top 10%",
                    ctd = ctd,
                    ctd_levels = ctd_levels,
                    prepare_ctd = FALSE, # Already prepared once above
                    gwas_sumstats_path = fake_gwas_ss, 
                    upstream_kb = upstream_kb,
                    downstream_kb = downstream_kb,
                    analysis_name = paste(ctd_name, suffix_top10, sep = "_"),
                    ctd_species = ctd_species,
                    force_new = force_new,
                    version = version,
                    verbose = verbose
                )
            }, error = function(e) {messager(e,v=verbose);NULL}
            )
        } 
        #### Merge results ####
        if (all(!is.null(ctAssocsLinear), !is.null(ctAssocsTop))) {
            messager("Merging linear and top10% results",
                v = verbose
            )
            ctAssocMerged <- merge_magma_results(
                ctAssoc1 = ctAssocsLinear,
                ctAssoc2 = ctAssocsTop
            )
        } 
        #### Conditional mode ####
        if (isTRUE(run_conditional)) {
            messager("=======",
                     "Calculating celltype associations: conditional mode",
                     "=======",
                v = verbose
            )
            ctCondAssocs <- tryCatch(expr = {
                    calculate_conditional_celltype_associations(
                        ctd = ctd,
                        EnrichmentMode = "Linear",
                        controlledAnnotLevel = controlledAnnotLevel,
                        prepare_ctd = FALSE, # Already prepared once above
                        gwas_sumstats_path = fake_gwas_ss, 
                        analysis_name = paste(ctd_name,
                            suffix_condition,
                            sep = "."
                        ),
                        upstream_kb = upstream_kb,
                        downstream_kb = downstream_kb,
                        controlTopNcells = controlTopNcells,
                        ctd_species = ctd_species,
                        force_new = force_new,
                        version = version,
                        verbose = verbose
                    )
                }, error = function(e) {messager(e,v=verbose);NULL}
            )
        }

        return(list(
            magma_dir = magma_dir,
            ctAssocsLinear = ctAssocsLinear,
            ctAssocsTop = ctAssocsTop,
            ctAssocMerged = ctAssocMerged,
            ctCondAssocs = ctCondAssocs
        ))
    }, mc.cores = nThread) |> `names<-`(basename(magma_dirs))
    #### Save results ####
    if (!is.null(save_dir)) {
        save_path <- file.path(
            save_dir, ctd_name,
            paste0("MAGMA_celltyping.", ctd_name, ".rds")
        )
        messager("Saving results ==>", save_path, v = verbose)
        dir.create(dirname(save_path), showWarnings = FALSE, recursive = TRUE)
        saveRDS(MAGMA_results, save_path)
    }
    return(MAGMA_results)
}
