#' Gather enrichment results 
#' 
#' Gather enrichment results from 
#' \link[MAGMA.Celltyping]{celltype_associations_pipeline} and converted them 
#' into harmonised, machine-readable tables.
#' 
#' @param MAGMA_results The output of
#'  \link[MAGMA.Celltyping]{celltype_associations_pipeline}.
#' @param level Which level in the CellTypeDataset (CTD) to show results for.
#' @param dataset_name [Optional] Name of the CellTypeDataset (CTD).
#' @param species Species that the CellTypeDataset (CTD) came from.
#' @param filetype Type of analysis to gather the results of.
#' These correspond to the modes of data analysis in 
#' \link[MAGMA.Celltyping]{celltype_associations_pipeline}:
#' \itemize{
#' \item{\code{"ctAssocsLinear"} : }{Linear mode results.}  
#' \item{\code{"ctAssocsTop"} : }{Top 10\% mode results.}  
#' \item{\code{"ctAssocMerged"} : }{Merged linear and top 10\% results.}  
#' \item{\code{"ctCondAssocs"} : }{Conditional results.}  
#' }  
#' 
#' \emph{Note:} Only those analyses that were actually run in 
#' \link[MAGMA.Celltyping]{celltype_associations_pipeline} can be retrieved.
#' @param q_thresh Multiple-testing corrected p-value (q-value) 
#' filtering threshold.
#' @param save_dir Directory to save the gathered results to.
#' @param verbose Print messages.
#' @inheritParams stats::p.adjust
#'
#' @examples
#' \dontrun{
#' #### Use precomputed results: see ?enrichment_results for details  ####
#' MAGMA_results <- MAGMA.Celltyping::enrichment_results
#' 
#' #### Merge results ####
#' merged_res <- MAGMA.Celltyping::merge_results(MAGMA_results)
#' }
#' @export
#' @importFrom data.table rbindlist fwrite
#' @importFrom dplyr mutate arrange
#' @importFrom stats p.adjust
#' @importFrom stringr str_split
merge_results <- function(MAGMA_results,
                          level = 1,
                          dataset_name = NULL,
                          species = "mouse",
                          filetype = "ctAssocMerged",
                          q_thresh = NULL,
                          method = "fdr",
                          save_dir = tempdir(),
                          verbose = TRUE) {
    #### Avoid confusing checks ####
    Celltype <- P <- FDR <- NULL;
    #### Iterate ####
    merged_results <- lapply(names(MAGMA_results), function(nm) {
        #### Check if the result exists ####
        if(is.null(MAGMA_results[[nm]][[filetype]])){
            messager("Results is NULL:",nm,"-",filetype,v=verbose)
            return(NULL)
        }
        #### Ensure requested level doesn't exceed available levels ####
        lvls <- grep("level", names(MAGMA_results[[nm]][[filetype]]),
                     value = TRUE)
        ## Account for the fact that older version of MAGMA.Celltyping 
        ## didn't label the levels as "level#".
        if(length(lvls>0)){
            level <- min(level,length(lvls))
        }
        cbind(
            GWAS = stringr::str_split(nm, ".annotated")[[1]][1],
            MAGMA_results[[nm]][[filetype]][[level]]$results
        )
    }) |>
        data.table::rbindlist() |>
        # Remove unlabeled cell clusters
        subset(!startsWith(Celltype, "X")) |>
        dplyr::mutate(
            FDR = stats::p.adjust(p = P, method = method),
            Celltype_id = Celltype,
            Celltype = gsub(paste(dataset_name, species, "[.]", sep = "|"),
                            " ", Celltype)
        ) |>
        dplyr::arrange(FDR)
    ### Save
    if (!is.null(save_dir)) {
        save_path <- file.path(save_dir, "MAGMA_celltyping.",
                               paste0(dataset_name, ".lvl", level, ".csv"))
        save_path <- fix_path(save_path)
        dir.create(dirname(save_path), showWarnings = FALSE, recursive = TRUE)
        messager("Saving full merged results to ==>", save_path, v=verbose)
        data.table::fwrite(merged_results, save_path)
    }
    ## Filter to only FDR-sig results
    if (!is.null(q_thresh)) {
        messager("Filtering results @ FDR<", q_thresh,v=verbose)
        merged_results <- subset(merged_results, FDR < q_thresh)
        messager(formatC(nrow(merged_results),big.mark = ","), 
                "significant results remaining.",
                v=verbose)
    }
    return(merged_results)
}
