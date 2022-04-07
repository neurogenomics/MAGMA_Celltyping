#' get data via \pkg{piggyback}
#'
#' @keywords internal
get_data <- function(fname,
                     repo = "neurogenomics/MAGMA_Celltyping",
                     storage_dir = tempdir(),
                     overwrite = FALSE,
                     check = FALSE) {
    tmp <- file.path(storage_dir, fname)
    if (!file.exists(tmp)) {
        Sys.setenv("piggyback_cache_duration" = 10)
        dir.create(storage_dir, showWarnings = FALSE, recursive = TRUE)
        piggyback::pb_download(
            file = fname,
            dest = storage_dir,
            repo = repo,
            overwrite = overwrite
        )
    }
    #### Check that download didn't fail due to bad credentials #####
    if(isTRUE(check)){
        get_data_check(tmp = tmp)    
    }
    return(tmp)
}
