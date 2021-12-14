#' Calculate cell type associations using MAGMA
#'
#' Has the option of running multiple analyses with a single function.
#' Assumes that you already have all MAGMA GWAS files precomputed.
#' Precomputed MAGMA GWAS files can be downloaded via the
#' \link[MAGMA.Celltyping]{import_magma_files} function.
#'
#' @param ctd Cell type data structure containing
#'  \code{specificity_quantiles}.
#' @param ctd_species Species name relevant to the cell type data.
#' See \link[EWCE]{list_species} for all available species.
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
#' @param version MAGMA version to use.
#' @param verbose Print messages.
#' @inheritParams calculate_celltype_associations
#' @inheritParams calculate_conditional_celltype_associations
#'
#' @returns A list containing the results of each selected
#' celltype associations analysis.
#'
#' @examples
#' magma_dirs <- MAGMA.Celltyping::import_magma_files(ids = c("ieu-a-298",
#'                                                             "ukb-b-6548"))
#' ctd <- ewceData::ctd()
#'
#' res <- MAGMA.Celltyping::celltype_associations_pipeline(
#'     ctd = ctd,
#'     ctd_levels = 1,
#'     ctd_name = "Zeisel2015",
#'     ctd_species = "mouse",
#'     magma_dirs = magma_dirs
#' )
#' @keywords main_function
#' @export
celltype_associations_pipeline <- function(ctd,
                                           ctd_levels = seq_len(length(ctd)),
                                           ctd_name,
                                           ctd_species = "mouse",
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
                                           version = NULL,
                                           verbose = TRUE) {
    #### Check MAGMA installation ####
    magma_check(version = version, 
                verbose = verbose)
    #### Establish vars in case some are not computed ####
    ctAssocsLinear <- NULL
    ctAssocsTop <- NULL
    ctCondAssocs <- NULL
    ctAssocMerged <- NULL
    #### prepare quantile groups ####
    # MAGMA.Celltyping can only use human GWAS
    {
        messager("Standardising CellTypeDataset.",v=verbose)
        output_species <- "human"
        ctd <- prepare_quantile_groups(
            ctd = ctd,
            input_species = ctd_species,
            output_species = output_species,
            verbose = FALSE
        )
        ctd_species <- output_species
    } 
    #### Iterate over GWAS datasets ####
    MAGMA_results <- lapply(magma_dirs, function(magma_dir) {
        messager(basename(magma_dir), v = verbose)
        #### Trick downstream functions into working with only MAGMA files ####
        fake_gwas_ss <- create_fake_gwas_path(magma_dir = magma_dir,
                                              upstream_kb = upstream_kb,
                                              downstream_kb = downstream_kb)
        #### Linear mode ####
        if (run_linear) {
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
            }, error = function(e) {NULL}
            )
        }

        #### Top 10% mode ####
        if (run_top10) {
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
            }, error = function(e) {NULL}
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
        if (run_conditional) {
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
                }, error = function(e) {NULL}
            )
        }

        return(list(
            magma_dir = magma_dir,
            ctAssocsLinear = ctAssocsLinear,
            ctAssocsTop = ctAssocsTop,
            ctAssocMerged = ctAssocMerged,
            ctCondAssocs = ctCondAssocs
        ))
    }) %>% `names<-`(basename(magma_dirs))


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
