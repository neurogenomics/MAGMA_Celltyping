#' get data via \pkg{piggyback}
#'
#' @keywords internal
get_data <- function(fname,
                     repo = "neurogenomics/MAGMA_Celltyping",
                     storage_dir = tempdir(),
                     overwrite = FALSE) {
    tmp <- file.path(storage_dir, fname)
    if (!file.exists(tmp)) {
        Sys.setenv("piggyback_cache_duration" = 10)
        piggyback::pb_download(
            file = fname,
            dest = storage_dir,
            repo = repo,
            overwrite = overwrite
        )
    }
    return(tmp)
}
