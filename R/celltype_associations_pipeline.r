#' Calculate cell type associations using MAGMA
#'
#' Has the option of running multiple analyses with a single function.
#' Assumes that you already have all MAGMA GWAS files precomputed.
#' Precomputed MAGMA GWAS files can be downloaded via the
#' \link[MAGMA.Celltyping]{import_magma_files} function.
#'
#' @param ctd Cell type data structure containing
#'  \code{specificity_quantiles}.
#' @param ctd_name Used in file names
#' @param magma_dirs Names of folders containing the
#' pre-computed MAGMA GWAS files.
#' \emph{NOTE}: Files within these folders must have the same naming scheme
#' as the folders themselves.
#' @param run_linear Run in linear mode.
#' @param run_top10 Run  in top 10\% mode.
#' @param run_condition Run in conditional mode.
#' @param suffix_linear This will be added to the linear results file name.
#' @param suffix_top10 This will be added to the top 10\% results file name.
#' @param suffix_condition This will be added to the
#'  conditional results file name.
#' @param save_dir Folder to save results in (\code{save_dir=NULL}
#'  to not save any results).
#' @param verbose Print messages.
#' @inheritParams calculate_celltype_associations
#' @inheritParams calculate_conditional_celltype_associations
#'
#' @returns A list containing the results of each selected
#' celltype associations analysis.
#'
#' @examples
#' \dontrun{
#' local_files <- MAGMA.Celltyping::import_magma_files()
#' magma_dirs <- unique(dirname(local_files))
#' ctd <- ewceData::ctd()
#'
#' res <- MAGMA.Celltyping::celltype_associations_pipeline(
#'     ctd = ctd,
#'     ctd_name = "Zeisel2015",
#'     magma_dirs = magma_dirs
#' )
#' }
#' @keywords main_function
#' @export
celltype_associations_pipeline <- function(ctd,
                                           ctd_name,
                                           magma_dirs,
                                           genome_ref_path = NULL,
                                           sctSpecies = "mouse",
                                           run_linear = TRUE,
                                           run_top10 = TRUE,
                                           run_condition = FALSE,
                                           upstream_kb = 35,
                                           downstream_kb = 10,
                                           suffix_linear = "linear",
                                           suffix_top10 = "top10",
                                           suffix_condition = "condition",
                                           controlTopNcells = 1,
                                           force_new = FALSE,
                                           save_dir = tempdir(),
                                           verbose = TRUE) {
    ## Establish vars in case some are not computed.
    ctAssocsLinear <- NULL
    ctAssocsTop <- NULL
    ctCondAssocs <- NULL
    ctAssocMerged <- NULL
    #### prepare quantile groups ####
    # MAGMA.Celltyping can only use human GWAS
    output_species <- "human"
    ctd <- prepare_quantile_groups(
        ctd = ctd,
        input_species = sctSpecies,
        output_species = output_species,
        verbose = verbose
    )
    sctSpecies <- output_species
    #### Prepare genome_ref ####
    genome_ref_path <- get_genome_ref(
        genome_ref_path = genome_ref_path,
        verbose = verbose
    )
    #### Iterate ####
    MAGMA_results <- lapply(magma_dirs, function(magma_dir) {
        messager(basename(magma_dir), v = verbose)
        fake_gwas_ss <- file.path(
            gsub("/MAGMA_Files", "", dirname(magma_dir)),
            gsub(
                paste0(".", upstream_kb, "UP.", downstream_kb, "DOWN"), "",
                basename(magma_dir)
            )
        )
        #### Linear mode ####
        if (run_linear) {
            messager("Calculating celltype associations: linear mode",
                v = verbose
            )
            ctAssocsLinear <- tryCatch(expr = {
                calculate_celltype_associations(
                    ctd = ctd,
                    prepare_ctd = FALSE, # Already prepared once above
                    gwas_sumstats_path = fake_gwas_ss,
                    genome_ref_path = genome_ref_path,
                    upstream_kb = upstream_kb,
                    downstream_kb = downstream_kb,
                    analysis_name = paste(ctd_name, suffix_linear, sep = "_"),
                    sctSpecies = sctSpecies,
                    force_new = force_new,
                    verbose = verbose
                )
            }, error = function(e) {
                message(e)
                return(NULL)
            })
        }

        #### Top 10% mode ####
        if (run_top10) {
            messager("Calculating celltype associations: top10% mode",
                v = verbose
            )
            ctAssocsTop <- tryCatch(expr = {
                calculate_celltype_associations(
                    ctd = ctd,
                    prepare_ctd = FALSE, # Already prepared once above
                    gwas_sumstats_path = fake_gwas_ss,
                    genome_ref_path = genome_ref_path,
                    EnrichmentMode = "Top 10%",
                    upstream_kb = upstream_kb,
                    downstream_kb = downstream_kb,
                    analysis_name = paste(ctd_name, suffix_top10, sep = "_"),
                    sctSpecies = sctSpecies,
                    force_new = force_new,
                    verbose = verbose
                )
            }, error = function(e) {
                message(e)
                return(NULL)
            })
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
        if (run_condition) {
            messager("Calculating celltype associations: conditional mode",
                v = verbose
            )
            ctCondAssocs <- tryCatch(
                {
                    calculate_conditional_celltype_associations(
                        ctd = ctd,
                        prepare_ctd = FALSE, # Already prepared once above
                        gwas_sumstats_path = fake_gwas_ss,
                        genome_ref_path = genome_ref_path,
                        analysis_name = paste(ctd_name,
                            suffix_condition,
                            sep = "."
                        ),
                        upstream_kb = upstream_kb,
                        downstream_kb = downstream_kb,
                        controlTopNcells = controlTopNcells,
                        sctSpecies = sctSpecies
                    )
                },
                error = function(e) {
                    message(e)
                    return(NULL)
                }
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
