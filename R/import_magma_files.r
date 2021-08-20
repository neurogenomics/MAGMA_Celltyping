#' Import remote MAGMA files
#' 
#' Import preprocessed MAGMA GWAS files from remote
#'  \href{https://github.com/neurogenomics/MAGMA_Files}{GitHub repository}.
#' Each MAGMA file will be put into its own folder of the same name as the file. 
#' \bold{WARNING}: 
#' 
#' @param download_dir Parent folder where you want to save the MAGMA files.
#' @param overwrite If the files have already been downloaded in the specified directory, 
#' these downloads will be skipped. Set \code{overwrite=T} to force them to be re-downloaded.
#' @param nThread Number of threads to parallelize downloading.
#' 
#' @return 
#' Paths to where the MAGMA files have been downloaded locally.
#' 
#' @examples 
#' local_files <- import_magma_files(download_dir=".")
#' @export 
import_magma_files <- function(download_dir=tempdir(),
                               file_types=c(".genes.raw",".genes.out"),
                               overwrite=FALSE,
                               nThread=parallel::detectCores()-2){ 
    magma_files <- github_list_files(creator="neurogenomics",
                                     repo="MAGMA_Files",
                                     branch="main",
                                     query = paste(file_types, collapse = "|"),
                                     return_download_api = TRUE)
    local_files <- github_download_files(filelist = magma_files, 
                                          download_dir = file.path(download_dir,"MAGMA_Files"),
                                          nThread = nThread, 
                                          overwrite = overwrite)
    return(local_files) 
}

