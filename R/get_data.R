#' Get remote data
#'
#' Download remotely stored data via \link[piggyback]{pb_download}.
#' @keywords internal
#' @inheritParams piggyback::pb_download
#' @importFrom tools R_user_dir
get_data <- function(fname,
                     repo = "neurogenomics/MAGMA_Celltyping",
                     storage_dir = tools::R_user_dir(
                       package = "MAGMA.Celltyping",
                       which = "cache"
                     ),
                     overwrite = TRUE,
                     check = FALSE
                     ){
  tmp <- fix_path(file.path(storage_dir, fname))
  requireNamespace("piggyback")
  requireNamespace("gh")
  Sys.setenv("piggyback_cache_duration" = 10)
  dir.create(storage_dir, showWarnings = FALSE, recursive = TRUE)
  .token <- gh::gh_token()
  if(as.character(.token)=="")  {
    message(paste0("Personal access token is missing and it is required to download ",
                   names(fname), "\n"))
    message("Please, configure git with R and try again \n")
    stop("Personal access token missing missing")
    } else {
      piggyback::pb_download(
        file = fname,
        dest = storage_dir,
        repo = repo,
        overwrite = overwrite,
        .token = .token
      )
    }

  #### Check that download didn't fail due to bad credentials #####
  if(isTRUE(check)){
    get_data_check(tmp = tmp)
    }
  return(tmp)
}


