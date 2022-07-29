#' Decompress a file
#' 
#' Decompress a gzipped (.gz) or bgzipped (.bgz) file. 
#' 
#' @param path_formatted Path to compressed file. 
#' @inheritParams get_example_gwas
#' @inheritParams R.utils::gunzip
#' @returns Path to decompressed file.
#' 
#' @keywords internal
decompress <- function(path_formatted,
                       remove = FALSE,
                       overwrite = FALSE,
                       storage_dir = tools::R_user_dir(
                           package = "MAGMA.Celltyping",
                           which = "cache"
                       ),
                       verbose = TRUE) {
    if (endsWith(path_formatted, ".gz")) {
        #### Decompress gzipped file #####
        destname <- file.path(
            storage_dir,
            gsub(".gz", "", basename(path_formatted))
        )
        messager("Saving decompressed copy of path_formatted ==> ", destname,
                 v=verbose)
        if (!file.exists(destname) | overwrite) {
            path_formatted2 <- R.utils::gunzip(path_formatted,
                destname = destname,
                remove = remove,
                overwrite = overwrite
            )
        } else {
            path_formatted2 <- destname
        }
    } else if (endsWith(path_formatted, ".bgz")) {
        #### Decompress bgzipped file #####
        destname <- file.path(
            storage_dir,
            sub(".bgz", "", basename(path_formatted))
        )
        messager("Saving decompressed copy of path_formatted ==> ", destname,
                 v=verbose)
        if (!file.exists(destname) | overwrite) {
            path_formatted2 <- R.utils::bunzip2(path_formatted,
                destname = destname,
                remove = remove,
                overwrite = overwrite
            )
        } else {
            path_formatted2 <- destname
        }
    } else {
        path_formatted2 <- path_formatted
    }
    return(as.character(path_formatted2))
}
