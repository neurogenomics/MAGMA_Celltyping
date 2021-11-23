#' Import preprocessed MAGMA files
#'
#' Import preprocessed MAGMA SNPs-to-genes mapping files for many GWAS. 
#' These files were generated using 
#' \link[MAGMA.Celltyping]{map_snps_to_genes} and are publicly available via the 
#' \href{https://github.com/neurogenomics/MAGMA_Files_Public}{
#' \emph{MAGMA_Files_Public} GitHub repository}. 
#' Metadata for each dataset (including trait descriptions) can be found here 
#' \href{https://github.com/neurogenomics/MAGMA_Files_Public/blob/master/metadata.csv}{here}.
#' 
#' @source \href{https://github.com/neurogenomics/MAGMA_Files_Public}{
#' \emph{MAGMA_Files_Public} GitHub repository}
#'
#' @param save_dir Parent folder where you want to save the MAGMA files.
#' @param ids [Optional] \href{https://gwas.mrcieu.ac.uk/}{OpenGWAS} dataset IDs. 
#' You see metadata for the GWAS 
#' \href{https://github.com/neurogenomics/MAGMA_Files_Public/blob/master/metadata.csv}{here}.
#' @param file_types MAGMA file types to import. 
#' Both ".genes.out" and ".genes.raw" are needed to run \pkg{MAGMA.Celltyping}
#' cell-type enrichment functions
#' @param overwrite If the files have already been downloaded in 
#' the specified directory,
#' these downloads will be skipped. Set \code{overwrite=TRUE} to force 
#' them to be re-downloaded (Default: \code{FALSE}).
#' @param return_dir Return a list of unique directory names instead of the 
#' full file paths (Default: \code{TRUE}).
#' @param nThread Number of threads to parallelise downloads across.
#' @param verbose Print messages.
#'
#' @returns Named vector of paths to downloaded MAGMA files (or directories).
#'
#' @examples
#' magma_dirs <- MAGMA.Celltyping::import_magma_files(ids = c("ieu-a-298",
#'                                                            "ukb-b-6548"))
#' @export
#' @importFrom stringr str_split
#' @importFrom parallel mclapply
import_magma_files <- function(save_dir = tempdir(),
                               ids = NULL, 
                               file_types = c(".genes.out",".genes.raw"),
                               overwrite = FALSE,
                               return_dir = TRUE,
                               nThread = 1,
                               verbose = TRUE) {
    #### Check what files are available #### 
    magma_files <- github_list_files(
        user = "neurogenomics",
        repo = "MAGMA_Files_Public",
        branch = "master",
        query = paste(file_types, collapse = "|"),
        return_download_api = TRUE,
        verbose = verbose
    ) 
    ##### Filter by dataset IDs ####
    if(!is.null(ids)){
        ## NOTE: Assumes that the dataset ID itself does NOT contain "."
        all_ids <- stringr::str_split(basename(magma_files),
                                      "[.]",simplify = TRUE)[,1] 
        bool_filter <- tolower(all_ids) %in% tolower(ids)
        all_ids <- all_ids[bool_filter]
        magma_files <- magma_files[bool_filter]
        messager("Filtering IDs to only",length(unique(all_ids)),
                 "requested dataset(s).",
                 v=verbose)
    }
    #### Download files locally ####
    local_files <- github_download_files(
        filelist = magma_files,
        save_dir = save_dir,
        nThread = nThread,
        overwrite = overwrite,
        verbose = verbose
    ) 
    #### Return ####
    if(return_dir){
        messager("Returning MAGMA directories.",v=verbose)
        magma_dirs <- unique(dirname(local_files))
        names(magma_dirs) <- basename(magma_dirs) 
        return(magma_dirs)
    } else {
        messager("Returning MAGMA gene.raw and gene.out file paths",v=verbose)
        ### Must include OpenGWAS ID + upstream/downtream params + file type
        ## bc we need all info in order to make list names unique.
        names(local_files) <- basename(local_files) 
        return(local_files)
    }
}
