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
#' @param return_tables Returns the files imported as tables (\code{TRUE}),
#' instead of as paths (default: \code{FALSE}). 
#' Overrides \code{return_dir} argument. 
#' @param nThread Number of threads to parallelise downloads across.
#' @param nested Return files as a list nested by \code{id} (default: \code{FALSE}).
#' @param verbose Print messages.
#'
#' @returns Named vector of paths to downloaded MAGMA files (or directories), 
#' or a \link[data.table]{data.table}.
#'
#' @examples
#' magma_dirs <- MAGMA.Celltyping::import_magma_files(ids = c("ieu-a-298"))
#' @export
#' @importFrom stringr str_split
#' @importFrom parallel mclapply
import_magma_files <- function(save_dir = tempdir(),
                               ids = NULL, 
                               file_types = c(".genes.out",".genes.raw"),
                               overwrite = FALSE,
                               return_dir = TRUE,
                               return_tables = FALSE,
                               nested = FALSE,
                               nThread = 1,
                               verbose = TRUE) {
    id <- NULL;
    file_types <- tolower(file_types) 
    #### Must be nested to return tables ####
    if(isTRUE(return_tables)) nested <- TRUE
    #### Use built-in data (for when there's no internet) ####
    if(all(ids=="ieu-a-298")){
        local_files <- get_example_magma_files(file_types = file_types,
                                               nested = nested,
                                               verbose = verbose) 
    } else {
        #### Check what files are available #### 
        meta <- import_magma_files_metadata(file_types = file_types, 
                                            use_local = TRUE,
                                            verbose = verbose)
        ##### Filter by dataset IDs ####
        if(!is.null(ids)){
            meta <- subset(meta, tolower(id) %in% tolower(ids))
            messager("Filtering IDs to only",
                     formatC(length(unique(meta$id)), big.mark = ","),
                     "requested dataset(s).",
                     v=verbose)
        }
        #### Get link to files ####
        magma_files <- import_magma_files_sort(meta = meta, 
                                               file_types = file_types,
                                               nested = FALSE,
                                               sorted = TRUE)
        #### Download files locally ####
        local_files <- github_download_files(
            filelist = magma_files,
            save_dir = fix_path(save_dir),
            nThread = nThread,
            overwrite = overwrite,
            verbose = verbose
        ) 
        if(isTRUE(nested)){
            local_files <- import_magma_files_renest(magma_files = local_files)
        }
    } 
    #### Return ####
    ## as tables 
    if(return_tables){ 
        messager("Returning MAGMA tables.",v=verbose)
         tbls <- import_magma_files_tables(local_files = local_files, 
                                           verbose = verbose)
         return(tbls)
    }
    ## as directories 
    if(return_dir){
        messager("Returning MAGMA directories.",v=verbose)
        magma_dirs <- unique(dirname(unlist(local_files)))
        names(magma_dirs) <- basename(magma_dirs) 
        magma_dirs <- fix_path(magma_dirs)
        return(magma_dirs)
    } else {
    ## as file paths
        messager("Returning MAGMA gene.raw and gene.out file paths",v=verbose)
        ### Must include OpenGWAS ID + upstream/downtream params + file type
        ## bc we need all info in order to make list names unique.
        if(isFALSE(nested)){
            local_files <- fix_path(local_files)    
        } 
        return(local_files)
    }
}
